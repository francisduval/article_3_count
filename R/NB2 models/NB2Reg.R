NB2Reg <- R6Class(
  classname = "NB2Reg",
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
    
    initialize = function(recipe) {
      self$recipe <- recipe
      self$predictors <- recipe$var_info %>% filter(role == "predictor") %>% pull(variable)
      self$response <- recipe$term_info %>% filter(role == "outcome") %>% pull(variable)
    },
    
    train = function(valid_df) {
      train_df_juiced <- juice(prep(self$recipe))
      valid_df_juiced <- bake(prep(self$recipe), new_data = valid_df)
      
      id_vars <- self$recipe$var_info$variable[self$recipe$var_info$role == "ID"]
      x_form <- names(select(train_df_juiced, -all_of(id_vars), -self$response))
      form <- reformulate(x_form, response = self$response)
      fit <- glm.nb(form, data = train_df_juiced)
      
      sum_p_2_pois <- function(mu) sum(map_dbl(0:30, ~ dpois(., lambda = mu) ^ 2))
      sum_p_2_nb2 <- function(mu, phi) sum(map_dbl(0:30, ~ dnbinom(., mu = mu, size = 1 / phi) ^ 2))
      
      # ---
      
      self$train_targets <- train_df_juiced[[self$response]]
      self$valid_targets <- valid_df_juiced[[self$response]]
      
      self$valid_res <- 
        valid_df %>% 
        select(vin, contract_start_date, nb_claims) %>% 
        mutate(
          mu = predict(fit, type = "response", newdata = valid_df_juiced),
          phi = 1 / fit$theta,
          mean = mu,
          sd = sqrt(mu + (mu ^ 2) / phi),
          prob = dnbinom(self$valid_targets, mu = mu, size = 1 / phi),
          norm_carre_p = map2_dbl(mu, phi, ~ sum_p_2_nb2(.x, phi = .y)),
          
          pred_naif = rep(mean(self$train_targets), length(self$valid_targets)),
          prob_naif = dpois(self$valid_targets, pred_naif),
          norm_carre_p_naif = map_dbl(pred_naif, sum_p_2_pois)
        )
      
      self$train_res <- 
        self$recipe$template %>% 
        select(vin, contract_start_date, nb_claims) %>% 
        mutate(
          mu = predict(fit, type = "response", newdata = train_df_juiced),
          phi = 1 / fit$theta,
          mean = mu,
          sd = sqrt(mu + (mu ^ 2) / phi),
          prob = dnbinom(self$train_targets, mu = mu, size = 1 / phi),
          norm_carre_p = map2_dbl(mu, phi, ~ sum_p_2_nb2(.x, phi = .y))
        )
      
      self$params_df <- tidy(fit)

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
