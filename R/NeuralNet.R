NeuralNet <- R6Class(
  classname = "NeuralNet",
  # inherit = ClassifMetrics,
  
  public =
    list(
      model_spec = NULL,
      dataset = NULL,
      train_df = NULL,
      valid_df = NULL,
      test_df = NULL,
      train_targets = NULL,
      valid_targets = NULL,
      test_targets = NULL,
      train_preds = NULL,
      test_preds = NULL,
      training = NULL,
      embeddings = NULL,
      
      initialize = function(model_spec, dataset, train_df, valid_df, test_df) {
        self$model_spec <- model_spec
        self$dataset <- dataset
        self$train_df <- train_df
        self$valid_df <- valid_df
        self$test_df <- test_df
      },
      
      train = function(input_size, nb_epochs = 1, lr = 0.01) {
        train_ds <- self$dataset(self$train_df)
        valid_ds <- self$dataset(self$valid_df)
        test_ds <- self$dataset(self$test_df)
        
        self$train_targets <- as.numeric(train_ds$y)
        self$valid_targets <- as.numeric(valid_ds$y)
        self$test_targets <- as.numeric(test_ds$y)
        
        train_dl <- dataloader(train_ds, batch_size = 128, shuffle = F)
        valid_dl <- dataloader(valid_ds, batch_size = 128, shuffle = F)
        
        # Fit on training set
        fit <- 
          self$model_spec %>%
          setup(
            loss = nn_poisson_nll_loss(log_input = F),
            optimizer = optim_adam,
            metrics = list(luz_metric_mse())
          ) %>%
          set_hparams(
            input_size = input_size
          ) %>% 
          set_opt_hparams(lr = lr) %>% 
          luz::fit(
            train_dl, 
            epochs = nb_epochs, 
            valid_data = valid_dl
          )
        
        # Predict on train and test sets
        self$train_preds <- as.double(fit$model$forward(train_ds[1:length(train_ds)]$x))
        self$test_preds <- as.double(fit$model$forward(test_ds[1:length(test_ds)]$x))
        
        # Save training process
        self$training <-
          tibble(
            train_loss = fit$records$metrics$train %>% map("loss") %>% flatten_dbl(),
            train_mse = fit$records$metrics$train %>% map("mse") %>% flatten_dbl(),
            valid_loss = fit$records$metrics$valid %>% map("loss") %>% flatten_dbl(),
            valid_mse = fit$records$metrics$valid %>% map("mse") %>% flatten_dbl()
          )
        
        invisible(self)
      },
      
      plot_training = function() {
        self$training %>%
          mutate(epochs = seq_along(train_loss)) %>%
          pivot_longer(cols = -"epochs") %>%
          separate(col = "name", into = c("dataset", "metric"), sep = "_") %>%
          ggplot(aes(x = epochs, y = value, col = dataset)) +
          geom_point(shape = 21) +
          geom_line() +
          scale_color_discrete(name = NULL) +
          ylab(NULL) +
          facet_grid(vars(metric), scales = "free_y")
      }
    )
)
