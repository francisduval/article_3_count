train_df <- tar_read(train)[1:500, ]
valid_df <- tar_read(valid)[1:500, ]


model <- PoissonMLP$new(PoissonCANN3L, DatasetNNCount)
model$train(train_df, valid_df, epochs = 2, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.3, n_1L = 128, n_2L = 64, n_3L = 32)

model$print_metrics()







target_vec <- c(0, 0, 1, 0, 2)
mu_vec <- c(0.02, 0.05, 0.04, 0.08, 0.03)



dev_one_obs <- function(mu, target) {
  x1 <- if_else(target == 0, 0, target * (log(target) - log(mu)))
  x2 <- target - mu
  2 * (x1 - x2)
}

dev_one_obs(0.05, 0)

rps_one_obs(0.05, 1)

map_dbl(0:20, ~ ppois(., lambda = 0.05))
map_dbl(0:20, ~ as.numeric(3 <= .))

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
