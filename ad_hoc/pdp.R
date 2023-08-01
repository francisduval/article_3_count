learn_mvnb <- tar_read(learn_mvnb)
test_mvnb <- tar_read(test_mvnb)

model <- MVNBMLP_imp$new(MVNBCANN3L, DatasetNNMVNB)
model$train(learn_mvnb, test_mvnb, epochs = 35, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.4, n_1L = 128, n_2L = 64, n_3L = 32)

pred <- function(mod, new_data) {
  new_data_ds <- DatasetNNMVNB_imp(new_data)
  mu <- as.double(mod$mu(new_data_ds[1:length(new_data_ds)]$x))
  alpha <- as.double(mod$alpha(new_data_ds[1:length(new_data_ds)]$x))
  gamma <- as.double(mod$gamma(new_data_ds[1:length(new_data_ds)]$x))
  
  moyenne <- mu * alpha / gamma
  
  return(moyenne)
}

pdp <- PartialDependence$new(model$trained_model, predict_fn = pred, data = test_mvnb)
pdp$add_variable("vma_16")
pdp$add_variable("vma_15")
pdp$add_variable("h_22")
pdp$add_variable("vma_5")
pdp$add_variable("h_2")
pdp$add_variable("p_2")
pdp$add_variable("vmo_8")
pdp$add_variable("vma_14")

p <- pdp$plot_all_pd(ncol = 4)

ggsave("figures/pdp.png", plot = p, width = 13)
