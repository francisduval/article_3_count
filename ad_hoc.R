train <- tar_read(train)
valid <- tar_read(valid)

model1 <- PoissonMLP$new(spec = PoissonCANN3L, dataset = DatasetNNCount)
model1$train(train[1:20000, ], valid[1:20000, ], epochs = 3, lr_start = 0.001, factor = 0.3, patience = 2, p = 0.2, batch = 256, n_1L = 16, n_2L = 8, n_3L = 4)




y <- torch_tensor(1)
lambda <- torch_tensor(1)

loss <- nn_poisson_nll_loss(log_input = F)
loss(lambda, y)
nnf_poisson_nll_loss(lambda, y, log_input = F)






preds_lambda <- seq(0.1, 10, 0.1)
target <- 1

aa <- map_dbl(preds_lambda, ~ poisson_log_loss_vec(truth = target, estimate = .x))
bb <- map_dbl(preds_lambda, ~ as.numeric(nnf_poisson_nll_loss(input = .x, target = target_2, log_input = F)))

tibble(
  x = preds_lambda,
  `poisson_log_loss_vec(1, lambda)` = aa,
  `nnf_poisson_nll_loss(1, lambda)` = bb
) %>% 
  pivot_longer(cols = `poisson_log_loss_vec(1, lambda)`:`nnf_poisson_nll_loss(1, lambda)`) %>%
  ggplot(aes(x = x, y = value, col = name)) +
  geom_line() +
  xlab("lambda") +
  ylab("-Log-vraisemblance") +
  labs(col = "Fonction utilisée")

