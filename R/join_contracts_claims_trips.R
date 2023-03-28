join_contracts_claims_trips <- function(contract_data, claim_data, trip_data) {
  contracts_and_trips_data <- 
    trip_data %>% 
    left_join(contract_data %>% select(vin:years_licensed), by = "vin") %>% 
    filter(date_start >= contract_start_date) %>% 
    filter(date_start < contract_end_date) 
  
  claim_data <- 
    claim_data %>% 
    mutate(
      cov_2_4_ind = as.numeric(cov_2_ind + cov_4_ind > 0),
      cov_1_2_3_4_5_6_ind = as.numeric(cov_1_ind + cov_2_ind + cov_3_ind + cov_4_ind + cov_5_ind + cov_6_ind > 0)
    ) %>% 
    group_by(policy_id, contract_start_date, vin) %>% 
    summarise(
      nb_claims_cov_1 = sum(cov_1_ind),
      nb_claims_cov_2 = sum(cov_2_ind),
      nb_claims_cov_3 = sum(cov_3_ind),
      nb_claims_cov_4 = sum(cov_4_ind),
      nb_claims_cov_5 = sum(cov_5_ind),
      nb_claims_cov_6 = sum(cov_6_ind),
      nb_claims_cov_2_4 = sum(cov_2_4_ind),
      nb_claims_cov_1_2_3_4_5_6 = sum(cov_1_2_3_4_5_6_ind)
    ) %>% 
    ungroup()
  
  contracts_claims_trips <- 
    left_join(contracts_and_trips_data, claim_data, by = c("policy_id", "contract_start_date", "vin")) %>%
    mutate_at(vars(starts_with("nb_claims_")), ~ replace_na(., 0)) %>% 
    mutate_at(vars(starts_with("nb_claims_")), .funs = list(temp = ~ factor(as.numeric(. > 0), levels = c("0", "1")))) %>% 
    rename_at(vars(ends_with("_temp")), ~ str_replace(., pattern = "nb_claims", replacement = "claim_ind")) %>% 
    rename_at(vars(ends_with("_temp")), ~ str_remove(., pattern = "_temp"))
  
  return(contracts_claims_trips)
}
