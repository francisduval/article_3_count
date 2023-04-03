DatasetNNCount <- 
  dataset(
    name = "DatasetNNCount",
    
    initialize = function(df) {
      data <- self$prepare_data(df)
      
      self$tele <- data$x$tele
      self$class <- data$x$class
      self$y <- data$y
    },
    
    .getitem = function(i) {
      list(
        x = list(
          tele = self$tele[i, ], 
          class = self$class[i, ]
        ), 
        y = self$y[i, ]
      )
    },
    
    .length = function() {
      self$y$size()[[1]]
    },
    
    prepare_data = function(df) {
      target_col <- as.matrix(df$nb_claims) 
      
      tele_cols <- 
        df %>%
        select(starts_with(c("h_", "p_", "vmo", "vma", "d_"))) %>%
        as.matrix()
      
      class_df <- select(df, expo:years_licensed, distance)
      
      rec_class <-
        recipe(~ ., data = class_df) %>%
        step_impute_median(commute_distance, years_claim_free) %>%
        step_other(all_nominal(), threshold = 0.05) %>%
        step_dummy(all_nominal()) %>%
        prep()
      
      class_cols <- juice(rec_class) %>% as.matrix()
      
      list(
        x = list(
          tele = torch_tensor(tele_cols),
          class = torch_tensor(class_cols)
        ),
        y = torch_tensor(target_col, dtype = torch_int())
      )
    }
  )
