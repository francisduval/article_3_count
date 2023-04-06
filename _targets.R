# Options =======================================================================================================================
tar_option_set(
  garbage_collection = T,
  memory = "transient",
  format = "qs",
  workspace_on_error = F,
  iteration = "list"
)


# Targets =======================================================================================================================
list(
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Préparation des données -----------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(contract_file, here("data", "Contrat_Nov2020.csv"), format = "file"),
  tar_target(contract_data, prepare_contract_data(contract_file)),
  tar_target(claim_data, prepare_claim_data(contract_data)),
  tar_files_input(trip_files, list.files(here("data"), pattern = "TRIP_VIN", full.names = T), format  = "file"),
  tar_target(trip_data, clean_trip_file(trip_files), pattern = map(trip_files), iteration = "vector"),
  tar_target(aug_trip_data, join_contracts_claims_trips(contract_data, claim_data, trip_data), pattern = map(trip_data), iteration = "vector"),
  
  tar_target(vins, unique(aug_trip_data$vin), pattern = map(aug_trip_data), iteration = "vector"),
  
  tar_target(vins_train, vins[1:30000]),
  tar_target(vins_valid, vins[30001:40000]),
  tar_target(vins_test, vins[40001:49671]),

  tar_target(atd_train, filter(aug_trip_data, vin %in% vins_train), pattern = map(aug_trip_data), iteration = "vector"),
  tar_target(atd_valid, filter(aug_trip_data, vin %in% vins_valid), pattern = map(aug_trip_data), iteration = "vector"),
  tar_target(atd_test, filter(aug_trip_data, vin %in% vins_test), pattern = map(aug_trip_data), iteration = "vector"),
  
  tar_target(train_class, compute_class_data(atd_train), pattern = map(atd_train), iteration = "vector"),
  tar_target(valid_class, compute_class_data(atd_valid), pattern = map(atd_valid), iteration = "vector"),
  tar_target(test_class, compute_class_data(atd_test), pattern = map(atd_test), iteration = "vector"),
  
  tar_target(train_tele, compute_tele_data(atd_train), pattern = map(atd_train), iteration = "vector"),
  tar_target(valid_tele, compute_tele_data(atd_valid), pattern = map(atd_valid), iteration = "vector"),
  tar_target(test_tele, compute_tele_data(atd_test), pattern = map(atd_test), iteration = "vector"),
  
  tar_target(train_nn, compute_nn_data(atd_train), pattern = map(atd_train), iteration = "vector"),
  tar_target(valid_nn, compute_nn_data(atd_valid), pattern = map(atd_valid), iteration = "vector"),
  tar_target(test_nn, compute_nn_data(atd_test), pattern = map(atd_test), iteration = "vector"),
  
  tar_target(train, join_class_tele_nn(train_class, train_tele, train_nn)),
  tar_target(valid, join_class_tele_nn(valid_class, valid_tele, valid_nn)),
  tar_target(test, join_class_tele_nn(test_class, test_tele, test_nn)),
  

  # -----------------------------------------------------------------------------------------------------------------------------
  # Modèles GLM -----------------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    rec_class,
    recipe(nb_claims ~ ., data = select(train, nb_claims:distance)) %>%
      step_impute_median(commute_distance, years_claim_free) %>%
      step_other(all_nominal(), threshold = 0.05) %>%
      step_dummy(all_nominal()) %>% 
      step_normalize()
  ),
  
  tar_target(
    rec_class_tele,
    recipe(nb_claims ~ ., data = select(train, nb_claims:frac_expo_fri_sat, -avg_daily_distance, -nb_trips)) %>%
      step_impute_median(commute_distance, years_claim_free) %>%
      step_other(all_nominal(), threshold = 0.05) %>%
      step_dummy(all_nominal()) %>% 
      step_normalize()
  ),

  tar_target(
    glm_poisson_class,
    {
      model <- PoissonGLM$new(rec_class, valid)
      model$train()
    }
  ),
  
  tar_target(
    glm_poisson_class_tele,
    {
      model <- PoissonGLM$new(rec_class_tele, valid)
      model$train()
    }
  )
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Réseaux de neurones ---------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  # tar_render(nn_poisson, here("RMarkdown", "nn_poisson", "nn_poisson.Rmd"))
)
  