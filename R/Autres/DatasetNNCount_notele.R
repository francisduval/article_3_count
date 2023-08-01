DatasetNNCount_notele <- 
  dataset(
    name = "DatasetNNCount_notele",
    
    initialize = function(df) {
      data <- self$prepare_data(df)
      
      self$x_mlp <- data$x_mlp
      self$x_skip <- data$x_skip
      self$y <- data$y
    },
    
    .getitem = function(i) {
      list(
        x = list(
          x_mlp = self$x_mlp[i, ],
          x_skip = self$x_skip[i, ]
        ),
        y = self$y[i, ]
      )
    },
    
    .length = function() {
      self$y$size()[[1]]
    },
    
    prepare_data = function(df) {
      target_col <- as.matrix(df$nb_claims)
      class_df <- select(df, expo:distance, -years_claim_free)
      
      rec_class <-
        recipe(~ ., data = class_df) %>%
        step_impute_median(commute_distance) %>%
        step_other(all_nominal(), threshold = 0.05) %>%
        step_dummy(all_nominal()) %>%
        prep()
      
      class_cols <- juice(rec_class) %>% as.matrix()
      
      list(
        x_mlp = torch_tensor(class_cols),
        x_skip = torch_tensor(class_cols),
        y = torch_tensor(target_col)
      )
    }
  )
