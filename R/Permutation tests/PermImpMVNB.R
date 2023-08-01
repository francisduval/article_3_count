PermImpMVNB <-
  R6::R6Class(
    "PermImpMVNB",
    public = list(
      model = NULL,
      pred_fn = NULL,
      new_data = NULL,
      
      original_performance = NULL,
      imp_df = NULL,
      
      initialize = function(model, pred_fn, new_data) {
        self$model <- model
        self$pred_fn <- pred_fn
        self$new_data <- new_data
      },
      
      compute_importance = function(n_perms = 2) {
        new_data_ds <- DatasetNNMVNB_imp(self$new_data)
        
        y <- as.numeric(new_data_ds$y)
        x_names <- names(new_data_ds$x_original)
        
        preds <- self$pred_fn(self$model, self$new_data)
        self$original_performance <- mean(-dnb2gen(y, preds$mu, preds$alpha, preds$gamma, ln = T))
        imp_mat <- matrix(NA, nrow = n_perms, ncol = length(x_names))
        colnames(imp_mat) <- x_names
        imp_tib <- as_tibble(imp_mat)
        
        # Perform permutations and compute importance scores
        for (i in 1:n_perms) {
          permuted_data <- self$new_data
          for (j in 1:length(x_names)) {
            permuted_data[x_names[j]] <- sample(permuted_data[[x_names[j]]])
            perm_preds <- self$pred_fn(self$model, permuted_data)
            perm_loss <- mean(-dnb2gen(y, perm_preds$mu, perm_preds$alpha, perm_preds$gamma, ln = T))
            imp_score <- perm_loss - self$original_performance
            imp_tib[i, j] <- imp_score
            permuted_data[x_names[j]] <- self$new_data[[x_names[j]]]
          }
        }

        self$imp_df <- imp_tib
      }
    )
  )
