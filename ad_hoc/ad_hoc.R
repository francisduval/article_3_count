train_mvnb <- tar_read(train_mvnb)[1:1000, ]
valid_mvnb <- tar_read(valid_mvnb)[1:250, ]

glm_mvnb_class_tele <- tar_read(glm_mvnb_class_tele)
glm_mvnb_class <- tar_read(glm_mvnb_class)
nn_mvnb <- tar_read(nn_mvnb)

nn_mvnb$valid_res$phi
glm_mvnb_class_tele$valid_res$phi
glm_mvnb_class$valid_res$phi

model <- MVNBMLP$new(MVNBCANN3L, DatasetNNMVNB)
model$train(train_mvnb, valid_mvnb, epochs = 20, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.3, n_1L = 128, n_2L = 64, n_3L = 32)

