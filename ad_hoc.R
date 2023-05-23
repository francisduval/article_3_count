rec_class_mvnb <- tar_read(rec_class_mvnb)
valid <- tar_read(valid)

model <- MVNBReg$new(rec_class_mvnb)
model$train(valid)



train_df <- tar_read(train)[1:10000, ]
valid_df <- tar_read(valid)

rec <- 
  recipe(nb_claims ~ ., data = select(train_df, nb_claims:distance, vin)) %>%
  update_role(vin, new_role = "ID") %>% 
  step_impute_median(commute_distance, years_claim_free) %>%
  step_other(all_nominal_predictors(), threshold = 0.05) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors())

model <- MVNBReg$new(rec)
model$train(valid_df)
model$print_metrics()






glm_mvnb_class <- tar_read(glm_mvnb_class)
glm_mvnb_class_tele <- tar_read(glm_mvnb_class_tele)

glm_nb2_class <- tar_read(glm_nb2_class)
glm_nb2_class_tele <- tar_read(glm_nb2_class_tele)

glm_poisson_class <- tar_read(glm_poisson_class)



modele <- MVNBMLP$new(spec = MVNBCANN3L, dataset = DatasetNNMVNB)
modele$train(train_df[1:500, ], valid_df[1:500, ], epochs = 10, lr_start = 0.01, factor = 0.5, patience = 2, n_1L = 16, n_2L = 8, n_3L = 4)
