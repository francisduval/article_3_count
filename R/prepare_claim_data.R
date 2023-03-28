prepare_claim_data <- function(contract_data_clean) {
  # Garder seulement les contrats (lignes) avec au moins une réclamation
  contracts_with_claim <- 
    contract_data_clean %>% 
    filter(!is.na(first_claim_id))
  
  # Transformer la base de données par contrats de manière à avoir une ligne par réclamation
  claims_first <- 
    contracts_with_claim %>% 
    select(
      vin, 
      policy_id,
      contract_start_date, 
      contract_end_date,
      contains("first")
    ) %>% 
    rename_all(~ str_replace_all(., "first_", ""))
  
  # ----------
  
  claims_second <- 
    contracts_with_claim %>% 
    select(
      vin, 
      policy_id,
      contract_start_date, 
      contract_end_date,
      contains("second")
    ) %>% 
    filter(!is.na(second_claim_id)) %>% 
    rename_all(~ str_replace_all(., "second_", ""))
  
  # ----------
  
  claims_third <- 
    contracts_with_claim %>% 
    select(
      vin, 
      policy_id,
      contract_start_date, 
      contract_end_date,
      contains("third")
    ) %>% 
    filter(!is.na(third_claim_id)) %>% 
    rename_all(~ str_replace_all(., "third_", ""))
  
  # ----------
  
  claims_fourth <- 
    contracts_with_claim %>% 
    select(
      vin, 
      policy_id,
      contract_start_date, 
      contract_end_date,
      contains("fourth")
    ) %>% 
    filter(!is.na(fourth_claim_id)) %>% 
    rename_all(~ str_replace_all(., "fourth_", ""))
  
  # ----------
  
  claims <- bind_rows(claims_first, claims_second, claims_third, claims_fourth)
  
  # Créer une base de données ayant une ligne par couverture
  cov_vec <- 
    claims %>%
    mutate_at(vars(claim_cov_1:claim_cov_4), as.character) %>% 
    pivot_longer(claim_cov_1:claim_cov_4, values_to = "cov", names_to = "name_cov") %>% 
    select(-starts_with("claim_cost"), -name_cov) %>% 
    filter(!is.na(cov)) %>% 
    pull(cov)
  
  coverages <- 
    claims %>%
    mutate_at(vars(claim_cost_1:claim_cost_4), as.character) %>% 
    pivot_longer(claim_cost_1:claim_cost_4, values_to = "cost", names_to = "name_cost") %>% 
    select(-starts_with("claim_cov"), -name_cost) %>% 
    filter(!is.na(cost)) %>% 
    mutate(
      cov = as.numeric(cov_vec),
      cost = as.numeric(cost)
    )
  
  # Créer des variables indicatrices et les coûts par couverture
  coverages %<>%
    dummy_cols("cov") %>%
    select(-cov) %>% 
    mutate_at(vars(contains("cov")), list(cost = ~. * coverages$cost)) %>% 
    select(-cost)
  
  # Retour à une ligne par réclamation
  claims_final <- 
    coverages %>% 
    group_by(claim_id) %>% 
    summarise(
      vin = first(vin),
      policy_id = first(policy_id),
      contract_start_date = first(contract_start_date),
      contract_end_date = first(contract_end_date),
      claim_date = first(claim_date),
      cov_1_ind = max(cov_1),
      cov_2_ind = max(cov_2),
      cov_3_ind = max(cov_3),
      cov_4_ind = max(cov_4),
      cov_5_ind = max(cov_5),
      cov_6_ind = max(cov_6),
      cov_1_cost = max(cov_1_cost),
      cov_2_cost = max(cov_2_cost),
      cov_3_cost = max(cov_3_cost),
      cov_4_cost = max(cov_4_cost),
      cov_5_cost = max(cov_5_cost),
      cov_6_cost = max(cov_6_cost),
    ) %>% 
    ungroup()
  
  return(claims_final)
}