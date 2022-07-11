functions {
    real[,,] run_model(int n_s, int n_d, int first_alpha,
                   int n_j, int n_y,
                   int[] periods,
                   real S0, real[] theta,
                   real zeta_shape,real zeta_scale,
                   real eta_shape,real eta_rate,
                   real R0_1, real tau_2,
                   real EfI_1,real EfI_2,
                   real[] c) {
        /////////////////////////////// (declare) derived quantities ///////////////////////////////
        real zeta[n_j];           // Serial intervals
        real eta[n_y];            // Exposure to hospitalisation
        
        real y_hat[n_d+1,n_s,(1+n_j+n_y+1)+5]; // +5 is for census_hosp_add,census_seropre, c_d, census_freq, and census_new_expo
        
        real tau[n_s];                // Relative transmission advantage
        real EfI[n_s];                // Effective infectious density
        real Lambda[n_s];             // Probability of exposure
        real Delta[n_s];              // Force of infection (probability of infection with a given strain)

        real S[n_d+1];                   // Susceptible
        real J[n_d+1, n_s, n_j];   // Infectious cases (non-hospital)
        real Y[n_d+1, n_s, n_y];   // Infectious cases (to be hospitalised)
                    
        real c_d[n_d+1];                 // contact parameter
        
        real census_hosp_add[n_d+1];       // Hospital admissions
            
        real census_seropre[n_d+1];                  // Seroprevalence
        real census_freq[n_d+1,n_s];           // Frequency of strains
        real census_new_expo[n_d+1,n_s];       // Newly exposed

        /////////////////////////////// Waiting time distributions ///////////////////////////////
        for (j in 1:n_j){
            zeta[j] = weibull_cdf(j,zeta_shape,zeta_scale) - weibull_cdf(j-1,zeta_shape,zeta_scale);
        }
        
        for (y in 1:n_y){
            eta[y] = (gamma_cdf(y, eta_shape,eta_rate) - gamma_cdf(y-1, eta_shape,eta_rate))/(1 - gamma_cdf(y-1, eta_shape,eta_rate));
        }
                        
        tau[1] = 1;
        tau[2] = tau_2;
        
        /////////////////////////////// initialise all containers to 0 ///////////////////////////////
        S = rep_array(0.0, n_d+1);
        J = rep_array(0.0, n_d+1, n_s, n_j);
        Y = rep_array(0.0, n_d+1, n_s, n_y);
                    
        c_d = rep_array(0.0,n_d+1);
        
        census_hosp_add = rep_array(0.0,n_d+1);
            
        census_seropre  = rep_array(0.0,n_d+1);
        census_freq     = rep_array(0.0,n_d+1,n_s);
        census_new_expo = rep_array(0.0,n_d+1,n_s);
    
        
        /////////////////////////////// initial conditions at day 1 ///////////////////////////////
        S[1] = S0;

        /////////////////////////////// Discrete-time model ///////////////////////////////
        for (d in 1:n_d){
            for(s in 1:n_s){
                //========================= Census =========================//
                census_new_expo[d,s] = J[d,s,1] + Y[d,s,1];
                
                for (y in 1:n_y){
                    census_hosp_add[d] += eta[y] * Y[d,s,y];
                }
                
                if(d==1 && s==1){
                    census_freq[d,s] = 1.0;
                }
                if (s == n_s){
                    if(sum(to_array_1d(J[d,,]))+sum(to_array_1d(Y[d,,]))!=0){
                        census_freq[d,1] = (sum(J[d,1,]) + sum(Y[d,1,]))/(sum(J[d,1,]) + sum(J[d,2,]) + sum(Y[d,1,]) + sum(Y[d,2,]));
                        census_freq[d,2] = (sum(J[d,2,]) + sum(Y[d,2,]))/(sum(J[d,1,]) + sum(J[d,2,]) + sum(Y[d,1,]) + sum(Y[d,2,]));
                    }
                
                    census_seropre[d] = 1 - S[d]/S0;
                }
                
                //========================= Probability of infection =========================//
                if(periods[d] == 0){
                    c_d[d] = 1.0;
                    
                    if(d==1 && s == 1){
                        // Importation of the original strain
                        EfI[1] = EfI_1;
                    } else{
                        EfI[s] = (sum(to_vector(zeta) .* to_vector(J[d,s,1:n_j])) + sum(to_vector(zeta) .* to_vector(Y[d,s,1:n_j])));
                    }
                    Lambda[s] = (tau[s]*EfI[s])/((S0/(tau[s]*R0_1))+tau[s]*EfI[s]);
    
                } else{
                    c_d[d] = c[periods[d]];

                    if(d==first_alpha && s == 2){
                        // Importation of B.1.1.7
                        EfI[2] = EfI_2;
                    } else{
                        EfI[s] = c[periods[d]]*(sum(to_vector(zeta) .* to_vector(J[d,s,1:n_j])) + sum(to_vector(zeta) .* to_vector(Y[d,s,1:n_j])));;
                    }
                    
                    Lambda[s] = (c[periods[d]]*tau[s]*EfI[s])/((S0/(tau[s]*R0_1))+c[periods[d]]*tau[s]*EfI[s]);
                }
            }
            
            for(s in 1:n_s){
                if(sum(Lambda)>0){
                    Delta[s] =  (Lambda[s] - prod(Lambda)) + (Lambda[s]/sum(Lambda)) * prod(Lambda);
                } else{
                    Delta[s] =  0.0;
                }
            }
            
            //========================= S =========================//
            S[d+1] = (1-sum(Delta)) * S[d];
            
            for(s in 1:n_s){
                //========================= J =========================//
                J[d+1,s,1]  = (1 - theta[s]) * Delta[s] * S[d];
                
                for (i in 2:n_j){
                    J[d+1,s,i] = J[d,s,i-1];
                }
                                
                //========================= Y =========================//
                Y[d+1,s,1]  =      theta[s] * Delta[s] * S[d];
                
                for (i in 2:n_y){
                    Y[d+1,s,i] = (1 - eta[i-1]) * Y[d,s,i-1];
                }
            }
        }
        
        y_hat[, 1, 1                                     ] = S;
        y_hat[,  ,(1         + 1):(1+n_j)                ] = J;
        y_hat[,  ,(1+n_j     + 1):(1+n_j+n_y)            ] = Y;
        y_hat[, 1,(1+n_j+n_y + 1)+1                      ] = census_hosp_add;
        y_hat[, 1,(1+n_j+n_y + 1)+2                      ] = census_seropre;
        y_hat[, 1,(1+n_j+n_y + 1)+3                      ] = c_d;
        y_hat[,  ,(1+n_j+n_y + 1)+4                      ] = census_freq;
        y_hat[,  ,(1+n_j+n_y + 1)+5                      ] = census_new_expo;
        
        return y_hat;
    }
}
