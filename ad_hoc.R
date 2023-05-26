train_df <- tar_read(train_mvnb)
valid_df <- tar_read(valid_mvnb)

model <- MVNBMLP$new(MVNBCANN3L, DatasetNNMVNB)
model$train(train_df[1:500, ], valid_df[1:500, ], epochs = 10, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.3, n_1L = 128, n_2L = 64, n_3L = 32)

model$print_metrics()


model <- PoissonMLP$new(PoissonCANN3L, DatasetNNCount)
model$train(train_df[1:500, ], valid_df[1:500, ], epochs = 10, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.3, n_1L = 128, n_2L = 64, n_3L = 32)

model$print_metrics()



train_df <- tar_read(train_mvnb)
valid_df <- tar_read(valid_mvnb)

modele <- MVNBMLP$new(spec = MVNBCANN3L, dataset = DatasetNNMVNB)
modele$train(train_df[1:500, ], valid_df[1:500, ], epochs = 10, lr_start = 0.01, factor = 0.5, patience = 2, n_1L = 16, n_2L = 8, n_3L = 4)
