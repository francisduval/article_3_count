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
  tar_target(learn, bind_rows(train, valid)),
  
  tar_target(train_mvnb, prepare_mvnb(train, mu_vec = glm_mvnb_class$train_res$mu)),
  tar_target(valid_mvnb, prepare_mvnb(valid, mu_vec = glm_mvnb_class$valid_res$mu)),
  tar_target(test_mvnb, prepare_mvnb(test, mu_vec = glm_mvnb_class_learn$valid_res$mu)),
  tar_target(learn_mvnb, bind_rows(train_mvnb, valid_mvnb)),
  
  tar_target(
    vars_class, 
    c(
      "expo",
      "annual_distance",
      "commute_distance",
      "conv_count_3_yrs_minor",
      "gender",
      "marital_status",
      "pmt_plan",
      "veh_age",
      "veh_use",
      # "years_claim_free",
      "years_licensed",
      "distance"
    )
  ),
  
  tar_target(
    vars_tele, 
    c(
      "avg_daily_nb_trips",
      "med_trip_avg_speed",
      "med_trip_distance",
      "med_trip_max_speed",
      "max_trip_max_speed",
      "prop_long_trip",
      "frac_expo_night",
      "frac_expo_noon",
      "frac_expo_evening",
      "frac_expo_peak_morning",
      "frac_expo_peak_evening",
      "frac_expo_mon_to_thu",
      "frac_expo_fri_sat"
    )
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Modèles GLM -----------------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  # Recipes for training set ----------------------------------------------------------------------------------------------------
  
  tar_target(
    rec_class,
    recipe(nb_claims ~ ., data = train) %>%
      update_role(-all_of(vars_class), -nb_claims, new_role = "ID") %>% 
      step_impute_median(commute_distance) %>%
      step_other(all_nominal_predictors(), threshold = 0.05) %>%
      step_dummy(all_nominal_predictors()) %>% 
      step_normalize(all_predictors())
  ),
  
  tar_target(
    rec_class_tele,
    recipe(nb_claims ~ ., data = train) %>%
      update_role(-all_of(vars_class), -all_of(vars_tele), -nb_claims, new_role = "ID") %>% 
      step_impute_median(commute_distance) %>%
      step_other(all_nominal_predictors(), threshold = 0.05) %>%
      step_dummy(all_nominal_predictors()) %>% 
      step_normalize(all_predictors())
  ),
  
  tar_target(
    rec_class_mvnb,
    recipe(nb_claims ~ ., data = train) %>%
      update_role(-all_of(vars_class), -nb_claims, new_role = "ID") %>% 
      step_impute_median(commute_distance) %>%
      step_other(all_nominal_predictors(), threshold = 0.05) %>%
      step_dummy(all_nominal_predictors()) %>% 
      step_normalize(all_predictors())
  ),
  
  tar_target(
    rec_class_tele_mvnb,
    recipe(nb_claims ~ ., data = train) %>%
      update_role(-all_of(vars_class), -all_of(vars_tele), -nb_claims, new_role = "ID") %>% 
      step_impute_median(commute_distance) %>%
      step_other(all_nominal_predictors(), threshold = 0.05) %>%
      step_dummy(all_nominal_predictors()) %>% 
      step_normalize(all_predictors())
  ),
  
  # Recipes for learning set ----------------------------------------------------------------------------------------------------
  
  tar_target(
    rec_class_learn,
    recipe(nb_claims ~ ., data = learn) %>%
      update_role(-all_of(vars_class), -nb_claims, new_role = "ID") %>% 
      step_impute_median(commute_distance) %>%
      step_other(all_nominal_predictors(), threshold = 0.05) %>%
      step_dummy(all_nominal_predictors()) %>% 
      step_normalize(all_predictors())
  ),
  
  tar_target(
    rec_class_tele_learn,
    recipe(nb_claims ~ ., data = learn) %>%
      update_role(-all_of(vars_class), -all_of(vars_tele), -nb_claims, new_role = "ID") %>% 
      step_impute_median(commute_distance) %>%
      step_other(all_nominal_predictors(), threshold = 0.05) %>%
      step_dummy(all_nominal_predictors()) %>% 
      step_normalize(all_predictors())
  ),
  
  tar_target(
    rec_class_mvnb_learn,
    recipe(nb_claims ~ ., data = learn) %>%
      update_role(-all_of(vars_class), -nb_claims, new_role = "ID") %>% 
      step_impute_median(commute_distance) %>%
      step_other(all_nominal_predictors(), threshold = 0.05) %>%
      step_dummy(all_nominal_predictors()) %>% 
      step_normalize(all_predictors())
  ),
  
  tar_target(
    rec_class_tele_mvnb_learn,
    recipe(nb_claims ~ ., data = learn) %>%
      update_role(-all_of(vars_class), -all_of(vars_tele), -nb_claims, new_role = "ID") %>% 
      step_impute_median(commute_distance) %>%
      step_other(all_nominal_predictors(), threshold = 0.05) %>%
      step_dummy(all_nominal_predictors()) %>% 
      step_normalize(all_predictors())
  ),
  
  # GLMs on training set --------------------------------------------------------------------------------------------------------

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
  
  # GLMs on learning set --------------------------------------------------------------------------------------------------------
  
  tar_target(
    glm_poisson_class_learn,
    {
      model <- PoissonGLM$new(rec_class_learn)
      model$train(test)
    }
  ),
  
  tar_target(
    glm_poisson_class_tele_learn,
    {
      model <- PoissonGLM$new(rec_class_tele_learn)
      model$train(test)
    }
  ),
  
  tar_target(
    glm_nb2_class_learn,
    {
      model <- NB2Reg$new(rec_class_learn)
      model$train(test)
    }
  ),
  
  tar_target(
    glm_nb2_class_tele_learn,
    {
      model <- NB2Reg$new(rec_class_tele_learn)
      model$train(test)
    }
  ),
  
  tar_target(
    glm_mvnb_class_learn,
    {
      model <- MVNBReg$new(rec_class_mvnb_learn)
      model$train(test)
    }
  ),
  
  tar_target(
    glm_mvnb_class_tele_learn,
    {
      model <- MVNBReg$new(rec_class_tele_mvnb_learn)
      model$train(test)
    }
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Tuning des hyperparamètres du réseau de neurones ----------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  # Grilles d'hyperparamètres ---------------------------------------------------------------------------------------------------
  
  tar_target(lr_start_grid, c(0.00001, 0.00005, 0.0001, 0.0005, 0.001)),
  tar_target(factor_grid, c(0.3, 0.4, 0.5)),
  tar_target(p_grid, c(0.2, 0.3, 0.4)),
  
  tar_target(
    df_grid,
    tibble(lr_start = lr_start_grid, factor = factor_grid, p = p_grid),
    pattern = cross(lr_start_grid, factor_grid, p_grid),
    iteration = "vector"
  ),
  
  # tar_target(
  #   poisson_tune,
  #   {
  #     model <- PoissonMLP$new(PoissonCANN3L, DatasetNNCount)
  #     model$train(train, valid, epochs = 30, lr_start = lr_start_grid, factor = factor_grid, p = p_grid, patience = 2, n_1L = 128, n_2L = 64, n_3L = 32)
  #     model
  #   },
  #   pattern = cross(lr_start_grid, factor_grid, p_grid)
  # ),
  # 
  # tar_target(
  #   nb2_tune,
  #   {
  #     model <- NB2MLP$new(NB2CANN3L, DatasetNNCount)
  #     model$train(train, valid, epochs = 30, lr_start = lr_start_grid, factor = factor_grid, p = p_grid, patience = 2, n_1L = 128, n_2L = 64, n_3L = 32)
  #     model
  #   },
  #   pattern = cross(lr_start_grid, factor_grid, p_grid)
  # ),
  
  # tar_target(
  #   mvnb_tune,
  #   {
  #     model <- MVNBMLP$new(MVNBCANN3L, DatasetNNMVNB)
  #     model$train(train_mvnb, valid_mvnb, epochs = 30, lr_start = lr_start_grid, factor = factor_grid, p = p_grid, patience = 2, n_1L = 128, n_2L = 64, n_3L = 32)
  #     model
  #   },
  #   pattern = cross(lr_start_grid, factor_grid, p_grid)
  # ),
  
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Réseaux de neurones ---------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  # CANNs on training set -------------------------------------------------------------------------------------------------------
  
  # tar_target(
  #   nn_poisson_valid,
  #   {
  #     model <- PoissonMLP$new(PoissonCANN3L, DatasetNNCount)
  #     model$train(train, valid, epochs = 100, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.4, n_1L = 128, n_2L = 64, n_3L = 32)
  #     model
  #   }
  # ),

  # tar_target(
  #   nn_nb2_valid,
  #   {
  #     model <- NB2MLP$new(NB2CANN3L, DatasetNNCount)
  #     model$train(train, valid, epochs = 100, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.4, n_1L = 128, n_2L = 64, n_3L = 32)
  #     model
  #   }
  # ),

  # tar_target(
  #   nn_mvnb_valid,
  #   {
  #     model <- MVNBMLP$new(MVNBCANN3L, DatasetNNMVNB)
  #     model$train(train_mvnb, valid_mvnb, epochs = 100, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.4, n_1L = 128, n_2L = 64, n_3L = 32)
  #     model
  #   }
  # ),
  
  # CANNs on learning set -------------------------------------------------------------------------------------------------------
  
  # tar_target(
  #   nn_poisson_test,
  #   {
  #     model <- PoissonMLP$new(PoissonCANN3L, DatasetNNCount)
  #     model$train(learn, test, epochs = 35, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.4, n_1L = 128, n_2L = 64, n_3L = 32)
  #     model
  #   }
  # ),

  tar_target(
    nn_nb2_test,
    {
      model <- NB2MLP$new(NB2CANN3L, DatasetNNCount)
      model$train(learn, test, epochs = 35, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.4, n_1L = 128, n_2L = 64, n_3L = 32)
      model
    }
  ),

  # tar_target(
  #   nn_mvnb_test,
  #   {
  #     model <- MVNBMLP$new(MVNBCANN3L, DatasetNNMVNB)
  #     model$train(learn_mvnb, test_mvnb, epochs = 35, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.4, n_1L = 128, n_2L = 64, n_3L = 32)
  #     model
  #   }
  # ),
  
  # CANNs on learning set (no telematics) ---------------------------------------------------------------------------------------
  
  # tar_target(
  #   nn_poisson_test_notele,
  #   {
  #     model <- PoissonMLP$new(PoissonCANN3L, DatasetNNCount_notele)
  #     model$train(learn, test, input_size_mlp = 15, epochs = 35, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.4, n_1L = 128, n_2L = 64, n_3L = 32)
  #     model
  #   }
  # ),

  tar_target(
    nn_nb2_test_notele,
    {
      model <- NB2MLP$new(NB2CANN3L, DatasetNNCount_notele)
      model$train(learn, test, input_size_mlp = 15, epochs = 35, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.4, n_1L = 128, n_2L = 64, n_3L = 32)
      model
    }
  )#,

  # tar_target(
  #   nn_mvnb_test_notele,
  #   {
  #     model <- MVNBMLP$new(MVNBCANN3L, DatasetNNMVNB_notele)
  #     model$train(learn_mvnb, test_mvnb, input_size_mlp = 15, epochs = 35, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.4, n_1L = 128, n_2L = 64, n_3L = 32)
  #     model
  #   }
  # ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Importance des variables ----------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  
  # tar_target(
  #   imp_mvnb,
  #   {
  #     # Entrainer le modèle
  #     model <- MVNBMLP_imp$new(MVNBCANN3L, DatasetNNMVNB)
  #     model$train(learn_mvnb, test_mvnb, epochs = 35, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.4, n_1L = 128, n_2L = 64, n_3L = 32)
  # 
  #     # Fonction permettant d'obtenir les prédictions à partir d'un modèle torch
  #     pred <- function(mod, new_data) {
  #       new_data_ds <- DatasetNNMVNB_imp(new_data)
  #       mu <- as.double(mod$mu(new_data_ds[1:length(new_data_ds)]$x))
  #       alpha <- as.double(mod$alpha(new_data_ds[1:length(new_data_ds)]$x))
  #       gamma <- as.double(mod$gamma(new_data_ds[1:length(new_data_ds)]$x))
  # 
  #       return(list(mu = mu, alpha = alpha, gamma = gamma))
  #     }
  # 
  #     # Calculer l'importance
  #     imp <- PermImpMVNB$new(model$trained_model, pred_fn = pred, new_data = test_mvnb)
  #     imp$compute_importance(n_perms = 100)
  # 
  #     return(imp$imp_df)
  #   }
  # )
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Rapports RMarkdown ----------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  # tar_render(nn_poisson, here("RMarkdown", "nn_poisson", "nn_poisson.Rmd"))
)
