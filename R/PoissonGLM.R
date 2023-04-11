PoissonGLM <- R6Class(
  classname = "PoissonGLM",
  inherit = CountMetrics,
  
  public = list(
    recipe = NULL, 
    train_df = NULL,
    train_df_juiced = NULL,
    valid_df = NULL,
    
    predictors = NULL,
    response = NULL,
    
    train_targets = NULL,
    valid_targets = NULL,
    
    params_df = NULL,
    var_imp = NULL,
    valid_preds = NULL,
    
    initialize = function(recipe, valid_df) {
      self$recipe <- recipe
      self$train_df <- recipe$template
      self$train_df_juiced <- juice(prep(recipe))
      self$valid_df <- valid_df
      
      self$predictors <- recipe$var_info %>% filter(role == "predictor") %>% pull(variable)
      self$response <- recipe$term_info %>% filter(role == "outcome") %>% pull(variable)
      
      self$train_targets <- self$train_df[[self$response]]
      self$valid_targets <- valid_df[[self$response]]
    },
    
    train = function() {
      spec <- poisson_reg(engine = "glm")
      wf <- workflow() %>% add_recipe(self$recipe) %>% add_model(spec)
      fit <- parsnip::fit(wf, data = self$train_df)
      self$params_df <- tidy(fit)
      self$var_imp <- extract_fit_parsnip(fit) %>% vip::vi()
      self$valid_preds <- predict(fit, new_data = self$valid_df)$.pred
      
      invisible(self)
    },
    
    plot_coefs = function() {
      df <-
        self$params_df %>%
        filter(term != "(Intercept)")
      
      df %>%
        mutate(Sign = if_else(estimate > 0, "+", "-")) %>%
        mutate(abs_estimate = abs(estimate)) %>%
        mutate(term = fct_reorder(term, abs_estimate)) %>%
        ggplot(aes(x = term, y = abs_estimate, fill = Sign)) +
        geom_col(alpha = 0.7) +
        xlab(NULL) +
        ylab("Absolute value of coefficient") +
        scale_fill_manual(values = c("#a61d21", "#00743F")) +
        coord_flip()
    },
    
    plot_var_imp = function() {
      self$var_imp %>% vip(num_features = 100, include_type = T) + geom_col(col = "black", fill = "white")
    },
    
    print = function() {
      cat("Régression Poisson non-pénalisée \n\n")
      cat("\tTraining set: ", nrow(self$train_df), " observations\n", sep = "")
      cat("\tValidation set: ", nrow(self$valid_df), " observations\n\n", sep = "")
      cat("\tVariable réponse: ", self$response, "\n\n", sep = "")
      cat("\tPrédicteurs:\n", glue("{self$predictors}\n"), sep = "\n\t")
      invisible(self)
    }
  )
)
