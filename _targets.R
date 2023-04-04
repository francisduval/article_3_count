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
  tar_target(vins_learn, vins[1:40000]),
  tar_target(vins_train, vins[1:25000]),
  tar_target(vins_valid, vins[25001:32500]),
  tar_target(vins_test, vins[32501:40000]),
  tar_target(vins_confirm, vins[40001:49671]),
  
  tar_target(atd_learn, filter(aug_trip_data, vin %in% vins_learn), pattern = map(aug_trip_data), iteration = "vector"),
  tar_target(atd_train, filter(aug_trip_data, vin %in% vins_train), pattern = map(aug_trip_data), iteration = "vector"),
  tar_target(atd_valid, filter(aug_trip_data, vin %in% vins_valid), pattern = map(aug_trip_data), iteration = "vector"),
  tar_target(atd_test, filter(aug_trip_data, vin %in% vins_test), pattern = map(aug_trip_data), iteration = "vector"),
  tar_target(atd_confirm, filter(aug_trip_data, vin %in% vins_confirm), pattern = map(aug_trip_data), iteration = "vector"),
  
  # tar_target(DatasetCount_learn, DatasetCount$new(atd_learn)),
  tar_target(DatasetCount_train, DatasetCount$new(atd_train)),
  tar_target(DatasetCount_valid, DatasetCount$new(atd_valid)),
  tar_target(DatasetCount_test, DatasetCount$new(atd_test)),
  # tar_target(DatasetCount_confirm, DatasetCount$new(atd_confirm)),
  
  # tar_target(learn_df, join_class_tele_nn(DatasetCount_learn)),
  tar_target(train_df, join_class_tele_nn(DatasetCount_train)),
  tar_target(valid_df, join_class_tele_nn(DatasetCount_valid)),
  tar_target(test_df, join_class_tele_nn(DatasetCount_test)),
  # tar_target(confirm_df, join_class_tele_nn(DatasetCount_confirm))
  
  tar_target(train_valid_df, bind_rows(train_df, valid_df)),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Modèles ---------------------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    rec_class,
    recipe(nb_claims ~ ., data = select(train_valid_df, nb_claims:years_licensed, distance)) %>%
      step_impute_median(commute_distance, years_claim_free) %>%
      step_other(all_nominal(), threshold = 0.05) %>%
      step_dummy(all_nominal())
  ),
  
  tar_target(glm_poisson, PoissonGLM$new(rec_class, test_df))
  
  # tar_render(nn_poisson, here("RMarkdown", "nn_poisson", "nn_poisson.Rmd"))
)
  