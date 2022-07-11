# predict hospitalisations from contact ratios predicted counterfactual scenario that 90% were wearing masks during first lockdown,
# using the epidemiological model

library(rstan)
library(MASS)
library(truncnorm)
library(lubridate)
library(zoo)
library(tidyverse)
library(gridExtra)
library(grid)
library(reshape2)
library(magrittr)
library(ggh4x)
library(gtools)
library(matrixStats)
library(splines)
library(mgcv)
library(boot)

############ Load data & model output ############ 

set.seed(0)

load("data/output1.rda")

extracted_output=rstan::extract(output1)

source("covid_input.R")
source("forpaper_alldata.R")

expose_stan_functions("covid_simulate.stan")
expose_stan_functions("bp_rng.stan")


# number of countries and number of days
n_a<-2
n_d<-nrow(roi)*7


thinned_index= floor(seq(1,length(extracted_output$lp__),length.out=100))
n_r = length(thinned_index)

#
# bootstrap this
hosppreds<-function(mydata, indices, extracted_output, n_s,first_alpha,
                    n_j,n_y,n_a, n_d, n_r, periods, S0,theta,zeta_shape,zeta_scale,
                    eta_shape ,eta_rate, thinned_index) 
  ## fit regression models
{ dat<-mydata[indices,]


lmall<-lm(log(contact)~ jur +google+ summer*mask + mask*google , data = dat) 
# predictions: what if scenario for each day in the original data
new<-mydata
new$mask<-0.9

new$whatif <-predict(lmall, newdata = new)

whatif_ni<-new$whatif[new$jur=="NI"]
whatif_i<-new$whatif[new$jur=="ROI"]


cr_draws = list(whatif_ni,whatif_i)

############ Storage ############ 


c_out=hosp_adm_out= array(NA,dim=c(n_a,n_d,n_r))

############ Simulate ############ 

for(a in 1:n_a){
  for(i in 1:n_r){
    predictedFit=run_model(n_s = n_s,
                           n_d = n_d,
                           first_alpha = first_alpha,
                           n_j = n_j,
                           n_y = n_y,
                           periods = periods[a,],
                           S0 = S0[a],
                           theta = theta,
                           zeta_shape = zeta_shape,
                           zeta_scale = zeta_scale,
                           eta_shape = eta_shape,
                           eta_rate = eta_rate,
                           R0_1 = extracted_output$R0_1[thinned_index[i]],
                           tau_2 = extracted_output$tau_2[thinned_index[i]],
                           EfI_1 = exp(extracted_output$log_EfI_1[thinned_index[i],a]),
                           EfI_2 = exp(extracted_output$log_EfI_2[thinned_index[i],a]),
                           c =  exp(cr_draws[[a]]))
    
    for(d in 1:n_d){
      hosp_adm_out[a,d,i] = exp(rnorm(1,log(predictedFit[[d]][[1]][[(1+n_j+n_y+1)+1]]+1),
                                      extracted_output$sigma_h[thinned_index[i]]))-1
      c_out[a,d,i] =                        predictedFit[[d]][[1]][[(1+n_j+n_y+1)+3]]
    }
  }  
}


# sum predicted hospitalisations for days 1:151 for Northern Ireland

# posterior median for NI and ROI for 151 days
medni<-medi<-rep(NA, 151)

for (d in 1:151) {
  medni[d]<-median(hosp_adm_out[1,d ,])
}

sumhospni<-sum(medni)

for (d in 1:151) {
  medi[d]<-median(hosp_adm_out[2,d ,])
}

sumhospi<-sum(medi[22:151])


diffni1<-1601-sumhospni
diffi1<-1521-sumhospi


c(sumhospni, sumhospi, diffni1, diffi1)

}


# run the bootstrap

bootreps<-10000

start.boot<-Sys.time()
hospboot<-boot(weekdata, hosppreds, R=bootreps, extracted_output=extracted_output, n_s=n_s,first_alpha=first_alpha,
               n_j=n_j,n_y=n_y, n_a= n_a, n_d=n_d, n_r= n_r, periods=periods, S0=S0,theta=theta,zeta_shape=zeta_shape,zeta_scale=zeta_scale,
               eta_shape=eta_shape ,eta_rate=eta_rate, thinned_index = thinned_index )
end.boot<-Sys.time()
# time taken
end.boot-start.boot

summary(hospboot)
# NI
boot.ci(hospboot, index = 1)

#ROI
boot.ci(hospboot, index = 2)

# difference NI
boot.ci(hospboot, index = 3)

# difference ROI
boot.ci(hospboot, index = 4)


