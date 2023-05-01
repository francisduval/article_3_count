NB2Reg <- R6Class(
  classname = "NB2Reg",
  inherit = NB2Metrics,
  
  public = list(
    recipe = NULL,
    predictors = NULL,
    response = NULL,
    
    train_targets = NULL,
    valid_targets = NULL,
    
    params_df = NULL,
    
    valid_mu = NULL,
    phi = NULL,
    
    valid_mu_naif = NULL,
    phi_naif = NULL,
    
    initialize = function(recipe) {
      self$recipe <- recipe
      self$predictors <- recipe$var_info %>% filter(role == "predictor") %>% pull(variable)
      self$response <- recipe$term_info %>% filter(role == "outcome") %>% pull(variable)
    },
    
    train = function(valid_df) {
      train_df_juiced <- juice(prep(self$recipe))
      valid_df_juiced <- bake(prep(self$recipe), new_data = valid_df)
      
      x_form <- names(select(train_df_juiced, -self$response))
      form <- reformulate(x_form, response = self$response)
      form_naif <- reformulate("1", response = self$response)
      
      fit <- glm.nb(form, data = train_df_juiced)
      fit_naif <- glm.nb(form_naif, data = train_df_juiced)
      
      self$params_df <- tidy(fit)
      self$valid_mu <- as.numeric(predict(fit, type = "response", newdata = valid_df_juiced))
      self$phi <- 1 / fit$theta
      
      self$valid_mu_naif <- as.numeric(predict(fit_naif, type = "response", newdata = valid_df_juiced))[1]
      self$phi_naif <- 1 / fit_naif$theta
      
      self$train_targets <- train_df_juiced[[self$response]]
      self$valid_targets <- valid_df_juiced[[self$response]]

      invisible(self)
    },
    
    print = function() {
      cat("Régression NB2 non-pénalisée \n\n")
      cat("\tTraining set: ", length(self$train_targets), " observations\n", sep = "")
      cat("\tValidation set: ", length(self$valid_targets), " observations\n\n", sep = "")
      cat("\tVariable réponse: ", self$response, "\n\n", sep = "")
      cat("\tPrédicteurs:\n", glue("{self$predictors}\n"), sep = "\n\t")
      invisible(self)
    }
  )
)
