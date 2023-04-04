PoissonGLM <- R6Class(
  classname = "PoissonGLM",
  inherit = CountMetrics,
  
  public = list(
    recipe = NULL, 
    train_df = NULL,
    train_df_juiced = NULL,
    test_df = NULL,
    
    predictors = NULL,
    response = NULL,
    
    train_targets = NULL,
    test_targets = NULL,
    
    params_df = NULL,
    test_preds = NULL,
    
    initialize = function(recipe, test_df) {
      self$recipe <- recipe
      self$train_df <- recipe$template
      self$train_df_juiced <- juice(prep(recipe))
      self$test_df <- test_df
      
      self$predictors <- recipe$var_info %>% filter(role == "predictor") %>% pull(variable)
      self$response <- recipe$term_info %>% filter(role == "outcome") %>% pull(variable)
      
      self$train_targets <- self$train_df[[self$response]]
      self$test_targets <- test_df[[self$response]]
    },
    
    train = function() {
      spec <- poisson_reg(engine = "glm")
      wf <- workflow() %>% add_recipe(self$recipe) %>% add_model(spec)
      fit <- parsnip::fit(wf, data = self$train_df)
      self$params_df <- tidy(fit)
      self$test_preds <- predict(fit, new_data = self$test_df)$.pred
      
      invisible(self)
    },
    
    print = function() {
      cat("Régression Poisson non-pénalisée \n\n")
      cat("\tTraining set: ", nrow(self$train_df), " observations\n", sep = "")
      cat("\tTesting set: ", nrow(self$test_df), " observations\n\n", sep = "")
      cat("\tVariable réponse: ", self$response, "\n\n", sep = "")
      cat("\tPrédicteurs:\n", glue("{self$predictors}\n"), sep = "\n\t")
      invisible(self)
    }
  )
)
