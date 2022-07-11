## Exposure to hospitalisation
# Incubation period
# McAloon meta_analysis
McAloon_mu = 1.63
McAloon_sigma = 0.5

x=seq(0,20,0.1)
y_McAloon=dlnorm(x,meanlog = McAloon_mu,sdlog = McAloon_sigma)

# Onset to hospitalisation
Pellis_gamma_mean = 5.14
Pellis_gamma_sd = 4.20

Pellis_gamma_alpha = 1.49771 # shape
Pellis_gamma_beta = 0.2913832 # rate

#mean(rgamma(1e5,shape = Pellis_gamma_alpha,rate = Pellis_gamma_beta))
#sd(rgamma(1e5,  shape = Pellis_gamma_alpha,rate = Pellis_gamma_beta))
y_Pellis=dgamma(x,shape = Pellis_gamma_alpha,rate = Pellis_gamma_beta)

Exposure_to_onset = rlnorm(1e5,meanlog = McAloon_mu,sdlog = McAloon_sigma)
Onset_to_hospitalisation = rgamma(1e5,shape = Pellis_gamma_alpha,rate = Pellis_gamma_beta)
Exposure_to_hospitalisation = Exposure_to_onset+Onset_to_hospitalisation
#median(Exposure_to_hospitalisation)
#plot(density(Exposure_to_hospitalisation))

# Approximate the joint distribution with gamma
hosp_delays_gamma_pars = fitdistr(Exposure_to_hospitalisation,"gamma")
#Exposure_to_hospitalisation_gamma = rgamma(1e5,shape = hosp_delays_gamma_pars$estimate[1],rate = hosp_delays_gamma_pars$estimate[2])
#points(density(Exposure_to_hospitalisation_gamma))

eta_shape = hosp_delays_gamma_pars$estimate[1]
eta_rate = hosp_delays_gamma_pars$estimate[2]

# 99% of hospital admissions happen before day n_y
n_y = ceiling(qgamma(0.99,shape = eta_shape, rate = eta_rate))


