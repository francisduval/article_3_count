compute_class_data <- function(aug_trip_data) {
  dist_df <- 
    aug_trip_data %>% 
    group_by(vin, contract_start_date) %>% 
    summarise(distance = sum(distance)) %>% 
    ungroup()
  
  class_df <- 
    aug_trip_data %>% 
    group_by(vin, contract_start_date) %>% 
    slice(1) %>% 
    select(vin, contract_start_date, nb_claims = nb_claims_cov_1_2_3_4_5_6, expo:years_licensed) %>% 
    ungroup() %>% 
    mutate(nb_claims = as.integer(nb_claims))
  
  left_join(class_df, dist_df, by = c("vin", "contract_start_date"))
}