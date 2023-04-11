PoissonNN <- 
  R6Class(
    classname = "PoissonNN",
    inherit = NNUtils,
    
    public =
      list(
        dataset = NULL,
        model_spec = NULL,
        train_df = NULL,
        valid_df = NULL,
        train_targets = NULL,
        valid_targets = NULL,
        train_preds = NULL,
        valid_preds = NULL,
        training = NULL,
        
        initialize = function(dataset, model_spec, train_df, valid_df) {
          self$dataset <- dataset
          self$model_spec <- model_spec
          self$train_df <- train_df
          self$valid_df <- valid_df
        },
        
        train = function(nb_epochs = 1, lr = 0.01, n_1L) {
          train_ds <- self$dataset(self$train_df)
          valid_ds <- self$dataset(self$valid_df)
          
          self$train_targets <- as.numeric(train_ds$y)
          self$valid_targets <- as.numeric(valid_ds$y)
          
          train_dl <- dataloader(train_ds, batch_size = 256, shuffle = F)
          valid_dl <- dataloader(valid_ds, batch_size = 256, shuffle = F)
          
          # Fit on training set
          fit <- 
            self$model_spec %>%
            setup(
              loss = nn_poisson_nll_loss(log_input = F),
              optimizer = optim_adam,
              metrics = list(luz_metric_mse())
            ) %>%
            set_opt_hparams(lr = lr) %>% 
            set_hparams(
              n_1L = n_1L
            ) %>% 
            luz::fit(
              train_dl, 
              epochs = nb_epochs, 
              valid_data = valid_dl,
              callbacks = luz_callback_early_stopping(patience = 3)
            )
          
          # Predict on train and valid sets
          self$train_preds <- as.double(fit$model$forward(train_ds[1:length(train_ds)]$x))
          self$valid_preds <- as.double(fit$model$forward(valid_ds[1:length(valid_ds)]$x))
          
          # Save training process
          self$training <-
            tibble(
              train_loss = fit$records$metrics$train %>% map("loss") %>% flatten_dbl(),
              train_mse = fit$records$metrics$train %>% map("mse") %>% flatten_dbl(),
              valid_loss = fit$records$metrics$valid %>% map("loss") %>% flatten_dbl(),
              valid_mse = fit$records$metrics$valid %>% map("mse") %>% flatten_dbl()
            )
          
          invisible(self)
        }
      )
  )
