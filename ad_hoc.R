valid_df <- tar_read(valid)[1:1000, ]
train_df <- tar_read(train)[1:5000, ]
train_df_mvnb <- tar_read(train_mvnb)[1:5000, ]
valid_df_mvnb <- tar_read(valid_mvnb)[1:5000, ]

vars_class <- 
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
    "years_claim_free",
    "years_licensed",
    "distance"
  )

rec_class <- 
  recipe(nb_claims ~ ., data = train_df) %>%
  update_role(-all_of(vars_class), -nb_claims, new_role = "ID") %>% 
  step_impute_median(commute_distance, years_claim_free) %>%
  step_other(all_nominal_predictors(), threshold = 0.05) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors())

model <- NB2Reg$new(rec_class)
model$train(valid_df)


rec_class_mvnb <- 
  recipe(nb_claims ~ ., data = train_df_mvnb) %>%
  update_role(-all_of(vars_class), -nb_claims, new_role = "ID") %>% 
  step_impute_median(commute_distance, years_claim_free) %>%
  step_other(all_nominal_predictors(), threshold = 0.05) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors())

model2 <- MVNBReg$new(rec_class_mvnb)
model2$train(valid_df_mvnb)
model2$print_metrics()

model3 <- PoissonGLM$new(rec_class)
model3$train(valid_df)
model3$print_metrics()
