functions {
    real bp_rng(real mu, real sigma_f) {
        return beta_proportion_rng(mu, (1.0/(sigma_f^2)));
    }
}
