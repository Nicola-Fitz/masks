## Generation intervals (Ferretti et al.)
zeta_shape = 3.2862 
zeta_scale = 6.1244 
# 99% of onward transmission happens before day n_j
n_j = ceiling(qweibull(0.99,shape = zeta_shape, scale = zeta_scale))
