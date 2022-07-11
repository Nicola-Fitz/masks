########## Load longitudinal data ##########
dat = read.csv("data/ireland_data.csv")
date=unique(dat$date)
days=1:length(date)
n_d = length(days)

########## Waiting time distributions ##########
source("covid_input_zeta.R")
source("covid_input_eta.R")

########## R0 prior ##########
# median of 2.79 and IQR of 1.16 (Liu et al. 2020)
prior_R0_1_mean = 2.79
prior_R0_1_sd = 0.86

########## Transmissibility prior ##########
# Table 6, PHE report
prior_tau_2_mean = 1.31
prior_tau_2_sd = 0.24

######### Other priors ######### 
# vaguely informative
prior_sigma_sd = 1.0
prior_EfI_sd = 10.0
prior_epsilon_sd = 1.0
  
######### Areas #########
areas = as.factor(c("NI","ROI"))
n_a = length(unique(areas))

periods = matrix(NA,nrow=n_a,ncol=n_d)
n_p = length(unique(unlist(dat[,"wk_periods"])))-1

retail_period_mean = matrix(0,nrow=n_a,ncol=n_p)

for(i in 1:n_a){
  periods[i,] = unname(unlist(dat[dat$area==areas[i],"wk_periods"]))
  retail_period_mean[i,] = tapply(dat[dat$area==areas[i],"retail_google"],dat[dat$area==areas[i],"wk_periods"],mean,na.rm=TRUE)[-1]
}
retail_period_mean[1,24:26] = retail_period_mean[1,23] # retail_period_mean[1,24:26] are missing

##########  Seroprevalence ##########
ROI_serop_index = which(!is.na(unlist(dat[dat$area=="ROI","serop"])))
ROI_serop = unlist(dat[dat$area=="ROI","serop"])

NI_serop_index = which(!is.na(unlist(dat[dat$area=="NI","serop"])))
NI_serop = unlist(dat[dat$area=="NI","serop"])

serop = matrix(NA,nrow=n_a,ncol=n_d)
for(i in 1:n_a){
  serop[i,] = as.numeric(unlist(dat[dat$area==areas[i],"serop"]))
}

##########  Susceptibles ##########
S0 = rep(NA,n_a)
for(i in 1:n_a){
  S0[i] = dat[dat$area==unique(areas)[i],"N"][1]
}

##########  Probability of hospitalisation given infection: theta ##########
n_s = 2
theta=rep(NA,n_s)
theta[1] = 0.026
theta[2] = theta[1]

########## Estimating the Frequency of B.1.1.7  ##########
n_reps = 100
pars = as.data.frame(matrix(NA,nrow=n_reps,ncol=4))
freq_alpha =  matrix(0,nrow=n_a,ncol=n_d)
first_alpha_temp = rep(NA,nrow=n_a)

freq_strain_func <- function(data, par) {
  dif = (par[1]/(1+exp(-par[2]*(data$t-par[3])))) - data$freq
  
  sum(dif[!is.na(dif)]^2)
}

for(a in 1:n_a){
  freq_alpha_temp = tibble(
    t=days, 
    date = date,
    freq = unlist(dat[dat$area==unique(areas)[a],"freq_alpha"])
  )
  freq_alpha_temp$freq[freq_alpha_temp$freq<0.25] = NA
  
  for(i in 1:n_reps){
    freq_alpha_optim <- optim(par = c(par1 = rtruncnorm(1,b = 1,0.9,0.09),
                                     par2 = rnorm(1,0.15,0.015),
                                     par3 = rnorm(1,315,3.15)), 
                             fn = freq_strain_func, data = freq_alpha_temp, method = "L-BFGS-B",
                             lower = c(par1 = 0.85, par2 = 0.03, par3 = 300), 
                             upper = c(par1 = 1.00, par2 = 0.3,  par3 = 320))
    for(j in 1:3){
      pars[i,j] = freq_alpha_optim$par[j]
    }
    pars[i,4] = freq_alpha_optim$value 
  }
  
  colnames(pars) = c("par1","par2","par3","fit")
  pars = pars[order(pars$fit),][1,]
  
  freq_alpha[a,] = freq_alpha_temp$freq
  freq_alpha[is.na(freq_alpha)]= 0
  
  first_alpha_temp[a] = which((pars$par1/(1+exp(-pars$par2*(freq_alpha_temp$t-pars$par3))))*unlist(dat[dat$area==unique(areas)[a],"cases"])>1)[1]
}

# We assume that the first incidence of B. 1.1.7. entered the island on day min(first_alpha_temp).
first_alpha = min(first_alpha_temp)

########## Cases, hospitalisation and date ##########
cases = hosp_adm = matrix(NA,nrow=n_a,ncol=n_d)
for(i in 1:n_a){
  cases[i,]     = unlist(dat[dat$area==unique(areas)[i],"cases"])
  hosp_adm[i,]  = unlist(dat[dat$area==unique(areas)[i],"hosp_admissions"])
}

cases = cases
hosp_adm = hosp_adm+1 # +1 because it gets logged during fitting

cases[is.na(cases)] = 0 # 0 is used as a magic number replacing NA in Stan
hosp_adm[is.na(hosp_adm)] = 0 # 0 is used as a magic number replacing NA in Stan

hosp_adm_cens = matrix(0,nrow=n_a,ncol=n_d)
for(i in 1:n_a){
  hosp_adm_cens[i,1:(which(hosp_adm[i,]>0)[1]-1)] = hosp_adm[i,which(hosp_adm[i,]>0)[1]]-1
}

########## Packing ########## 
model_input = list(n_s = n_s,
                   n_d = n_d,
                   n_a = n_a,
                   areas = as.numeric(areas),
                   first_alpha = first_alpha,
                   n_j = n_j,   
                   n_y = n_y,
                   n_p = n_p,
                   periods = periods,
                   hosp_adm = hosp_adm,
                   hosp_adm_cens = hosp_adm_cens,
                   freq_alpha = freq_alpha,
                   S0 = S0,
                   theta = theta,
                   zeta_shape = zeta_shape,
                   zeta_scale = zeta_scale,
                   eta_shape = eta_shape,
                   eta_rate = eta_rate,
                   prior_R0_1_mean = prior_R0_1_mean,
                   prior_R0_1_sd = prior_R0_1_sd,
                   prior_tau_2_mean = prior_tau_2_mean,
                   prior_tau_2_sd = prior_tau_2_sd,
                   prior_sigma_sd = prior_sigma_sd,
                   prior_EfI_sd = prior_EfI_sd,
                   prior_epsilon_sd = prior_epsilon_sd)

