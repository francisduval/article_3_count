compute_sum_past_claims <- function(data) {
  data %>% 
    group_by(vin) %>% 
    mutate(sum_past_claims = cumsum(nb_claims) - nb_claims) %>% 
    ungroup() %>% 
    mutate(mu = 0.07)
}