compute_sum_past_mu <- function(data) {
  data %>% 
    group_by(vin) %>% 
    mutate(sum_past_mu = cumsum(mu) - mu) %>% 
    ungroup()
}