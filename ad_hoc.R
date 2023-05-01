train <- tar_read(train)
valid <- tar_read(valid)



model <- NB2MLP$new(NB2CANN3L, DatasetNNCount)
model$train(train, valid, epochs = 20, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.3, n_1L = 128, n_2L = 64, n_3L = 32)
model




x <- glm.nb(nb_claims ~ 1, data = train)
summary(x)

rec_class <- tar_read(rec_class)


model_1 <- PoissonGLM$new(rec_class)
model_1$train(valid)

model_2 <- NB2Reg$new(rec_class)
model_2$train(valid)



model <- NB2MLP$new(NB2CANN3L, DatasetNNCount)
model$train(train, valid, epochs = 1, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.4, n_1L = 128, n_2L = 64, n_3L = 32)
model




model <- NB2MLP$new(NB2CANN3L, DatasetNNCount)
model$train(train[1:2048, ], valid[1:512, ], epochs = 5, lr_start = 0.001, factor = 0.3, patience = 2, batch = 256, n_1L = 16, n_2L = 8, n_3L = 4)


x <- glm.nb(nb_claims ~ 1, train)
summary(x)
