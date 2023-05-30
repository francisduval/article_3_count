PoissonGLM <- R6Class(
  classname = "PoissonGLM",
  inherit = CountMetrics,
  
  public = list(
    recipe = NULL,
    predictors = NULL,
    response = NULL,
    
    train_targets = NULL,
    valid_targets = NULL,
    
    train_res = NULL,
    valid_res = NULL,
    
    params_df = NULL,
    var_imp = NULL,
    
    initialize = function(recipe) {
      self$recipe <- recipe
      self$predictors <- recipe$var_info %>% filter(role == "predictor") %>% pull(variable)
      self$response <- recipe$term_info %>% filter(role == "outcome") %>% pull(variable)
    },
    
    train = function(valid_df) {
      train_df <- self$recipe$template
      
      spec <- poisson_reg(engine = "glm")
      wf <- workflow() %>% add_recipe(self$recipe) %>% add_model(spec)
      fit <- parsnip::fit(wf, data = train_df)
      
      sum_p_2 <- function(mu) sum(map_dbl(0:30, ~ dpois(., lambda = mu) ^ 2))
      
      self$train_targets <- train_df[[self$response]]
      self$valid_targets <- valid_df[[self$response]]
      
      self$valid_res <- 
        valid_df %>% 
        select(vin, contract_start_date, nb_claims) %>% 
        mutate(
          mu = predict(fit, new_data = valid_df)$.pred,
          mean = mu,
          sd = sqrt(mu),
          prob = dpois(self$valid_targets, mu),
          norm_carre_p = map_dbl(mu, sum_p_2),
          
          pred_naif = rep(mean(self$train_targets), length(self$valid_targets)),
          prob_naif = dpois(self$valid_targets, pred_naif),
          norm_carre_p_naif = map_dbl(pred_naif, sum_p_2)
        )
      
      self$train_res <- 
        self$recipe$template %>% 
        select(vin, contract_start_date, nb_claims) %>% 
        mutate(
          mu = predict(fit, new_data = train_df)$.pred,
          mean = mu,
          sd = sqrt(mu),
          prob = dpois(self$train_targets, mu),
          norm_carre_p = map_dbl(mu, sum_p_2)
        )
      
      self$params_df <- tidy(fit)
      self$var_imp <- extract_fit_parsnip(fit) %>% vip::vi()
      
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
