prepare_mvnb <- function(data, mu_vec) {
  data %>% 
    mutate(mu = mu_vec) %>% 
    group_by(vin) %>% 
    mutate(sum_past_mu = cumsum(mu) - mu) %>% 
    mutate(sum_past_claims = cumsum(nb_claims) - nb_claims) %>% 
    ungroup()
}