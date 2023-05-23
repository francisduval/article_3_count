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
  
  tar_target(train_mvnb, train %>% compute_sum_past_claims() %>% compute_sum_past_mu()),
  tar_target(valid_mvnb, valid %>% compute_sum_past_claims() %>% compute_sum_past_mu()),
  tar_target(test_mvnb, test %>% compute_sum_past_claims() %>% compute_sum_past_mu()),
  

  # -----------------------------------------------------------------------------------------------------------------------------
  # Modèles GLM -----------------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    rec_class,
    recipe(nb_claims ~ ., data = select(train, nb_claims:distance)) %>%
      step_impute_median(commute_distance, years_claim_free) %>%
      step_other(all_nominal(), threshold = 0.05) %>%
      step_dummy(all_nominal()) %>% 
      step_normalize(all_predictors())
  ),
  
  tar_target(
    rec_class_mvnb,
    recipe(nb_claims ~ ., data = select(train, nb_claims:distance, vin)) %>%
      update_role(vin, new_role = "ID") %>% 
      step_impute_median(commute_distance, years_claim_free) %>%
      step_other(all_nominal_predictors(), threshold = 0.05) %>%
      step_dummy(all_nominal_predictors()) %>% 
      step_normalize(all_predictors())
  ),
  
  tar_target(
    rec_class_tele,
    recipe(nb_claims ~ ., data = select(train, nb_claims:frac_expo_fri_sat, -avg_daily_distance, -nb_trips)) %>%
      step_impute_median(commute_distance, years_claim_free) %>%
      step_other(all_nominal(), threshold = 0.05) %>%
      step_dummy(all_nominal()) %>% 
      step_normalize(all_predictors())
  ),
  
  tar_target(
    rec_class_tele_mvnb,
    recipe(nb_claims ~ ., data = select(train, nb_claims:frac_expo_fri_sat, -avg_daily_distance, -nb_trips, vin)) %>%
      update_role(vin, new_role = "ID") %>% 
      step_impute_median(commute_distance, years_claim_free) %>%
      step_other(all_nominal_predictors(), threshold = 0.05) %>%
      step_dummy(all_nominal_predictors()) %>% 
      step_normalize(all_predictors())
  ),

  tar_target(
    glm_poisson_class,
    {
      model <- PoissonGLM$new(rec_class)
      model$train(valid)
    }
  ),
  
  tar_target(
    glm_poisson_class_tele,
    {
      model <- PoissonGLM$new(rec_class_tele)
      model$train(valid)
    }
  ),
  
  tar_target(
    glm_nb2_class,
    {
      model <- NB2Reg$new(rec_class)
      model$train(valid)
    }
  ),
  
  tar_target(
    glm_nb2_class_tele,
    {
      model <- NB2Reg$new(rec_class_tele)
      model$train(valid)
    }
  ),
  
  tar_target(
    glm_mvnb_class,
    {
      model <- MVNBReg$new(rec_class_mvnb)
      model$train(valid)
    }
  ),
  
  tar_target(
    glm_mvnb_class_tele,
    {
      model <- MVNBReg$new(rec_class_tele_mvnb)
      model$train(valid)
    }
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Tuning des hyperparamètres du réseau de neurones ----------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  # Grilles d'hyperparamètres ---------------------------------------------------------------------------------------------------
  
  tar_target(n_1L_grid2, c(16, 32, 64, 128)),
  tar_target(n_2L_grid2, c(8, 16, 32, 64)),
  tar_target(n_3L_grid2, c(4, 8, 16, 32)),
  tar_target(lr_start_grid, c(0.001, 0.0005, 0.0001, 0.00005, 0.00001)),
  tar_target(factor_grid, c(0.5, 0.4, 0.3)),
  tar_target(p_grid, c(0.2, 0.3, 0.4)),
  tar_target(batch_grid, c(128, 256, 512, 1024)),
  
  tar_target(
    lr_start_factor_grid,
    tibble(lr_start = lr_start_grid, factor = factor_grid),
    pattern = cross(lr_start_grid, factor_grid),
    iteration = "vector"
  ),
  
  tar_target(
    p_lr_start_grid,
    tibble(p = p_grid, lr_start = lr_start_grid),
    pattern = cross(p_grid, lr_start_grid),
    iteration = "vector"
  ),
  
  # Tuning ----------------------------------------------------------------------------------------------------------------------
  
  # tar_target(
  #   PoissonCANN1L_tune,
  #   {
  #     model <- PoissonMLP$new(PoissonCANN1L, DatasetNNCount)
  #     model$train(train, valid, epochs = 30, lr_start = 0.01, factor = 0.5, patience = 1, n_1L = n_1L_grid)
  #     model
  #   },
  #   pattern = map(n_1L_grid)
  # ),
  # 
  # tar_target(
  #   PoissonCANN2L_tune,
  #   {
  #     model <- PoissonMLP$new(PoissonCANN2L, DatasetNNCount)
  #     model$train(train, valid, epochs = 30, lr_start = 0.01, factor = 0.5, patience = 1, n_1L = n_1L_grid, n_2L = n_2L_grid)
  #     model
  #   },
  #   pattern = map(n_1L_grid, n_2L_grid)
  # ),
  # 
  # tar_target(
  #   PoissonCANN3L_tune,
  #   {
  #     model <- PoissonMLP$new(PoissonCANN3L, DatasetNNCount)
  #     model$train(train, valid, epochs = 30, lr_start = 0.01, factor = 0.5, patience = 1, n_1L = n_1L_grid, n_2L = n_2L_grid, n_3L = n_3L_grid)
  #     model
  #   },
  #   pattern = map(n_1L_grid, n_2L_grid, n_3L_grid)
  # ),
  
  # tar_target(
  #   plateau_tune,
  #   {
  #     model <- PoissonMLP$new(PoissonCANN3L, DatasetNNCount)
  #     model$train(train, valid, epochs = 10, lr_start = lr_start_grid, factor = factor_grid, patience = 2, n_1L = 16, n_2L = 8, n_3L = 4)
  #     model
  #   },
  #   pattern = cross(lr_start_grid, factor_grid)
  # ),
  # 
  # tar_target(
  #   dropout_tune,
  #   {
  #     model <- PoissonMLP$new(PoissonCANN3L_DO, DatasetNNCount)
  #     model$train(train, valid, epochs = 20, lr_start = 0.001, factor = 0.3, patience = 2, n_1L = 16, n_2L = 8, n_3L = 4, p = p_grid)
  #     model
  #   },
  #   pattern = map(p_grid)
  # ),
  
  # tar_target(
  #   batch_tune,
  #   {
  #     model <- PoissonMLP$new(PoissonCANN3L, DatasetNNCount)
  #     model$train(train, valid, epochs = 20, lr_start = 0.001, factor = 0.3, patience = 2, batch = batch_grid, n_1L = 16, n_2L = 8, n_3L = 4, p = 0.2)
  #     model
  #   },
  #   pattern = map(batch_grid)
  # ),
  
  tar_target(n_1L_grid, c(16, 32, 64, 128, 16, 64, 128, 64)),
  tar_target(n_2L_grid, c(8, 16, 32, 64, 16, 64, 128, 128)),
  tar_target(n_3L_grid, c(4, 8, 16, 32, 16, 64, 128, 32)),
  tar_target(
    hu_tune,
    {
      model <- PoissonMLP$new(PoissonCANN3L, DatasetNNCount)
      model$train(train, valid, epochs = 20, lr_start = 0.001, factor = 0.3, patience = 2, batch = 256, p = 0.2, n_1L = n_1L_grid, n_2L = n_2L_grid, n_3L = n_3L_grid)
      model
    },
    pattern = map(n_1L_grid, n_2L_grid, n_3L_grid)
  ),
  
  tar_target(
    big_tune,
    {
      model <- PoissonMLP$new(PoissonCANN3L, DatasetNNCount)
      model$train(train, valid, epochs = 20, lr_start = p_lr_start_grid[[2]], factor = 0.3, patience = 2, batch = 256, p = p_lr_start_grid[[1]], n_1L = 128, n_2L = 64, n_3L = 32)
      model
    },
    pattern = map(p_lr_start_grid)
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Réseaux de neurones ---------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    nn_poisson,
    {
      model <- PoissonMLP$new(PoissonCANN3L, DatasetNNCount)
      model$train(train, valid, epochs = 22, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.3, n_1L = 128, n_2L = 64, n_3L = 32)
      model
    }
  ),
  
  tar_target(
    nn_nb2,
    {
      model <- NB2MLP$new(NB2CANN3L, DatasetNNCount)
      model$train(train, valid, epochs = 22, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.3, n_1L = 128, n_2L = 64, n_3L = 32)
      model
    }
  )
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Rapports RMarkdown ----------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  # tar_render(nn_poisson, here("RMarkdown", "nn_poisson", "nn_poisson.Rmd"))
)
