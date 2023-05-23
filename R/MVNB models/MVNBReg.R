MVNBReg <- R6Class(
  classname = "MVNBReg",
  inherit = MVNBMetrics,
  
  public = list(
    recipe = NULL,
    predictors = NULL,
    response = NULL,
    
    train_targets = NULL,
    valid_targets = NULL,
    
    betas = NULL,
    phi = NULL,
    
    betas_naif = NULL,
    phi_naif = NULL,
    
    valid_mu = NULL,
    valid_alpha = NULL,
    valid_gamma = NULL,
    
    valid_mu_naif = NULL,
    valid_alpha_naif = NULL,
    valid_gamma_naif = NULL,
    
    valid_prime = NULL,
    valid_prime_naif = NULL,
    
    initialize = function(recipe) {
      self$recipe <- recipe
      self$predictors <- recipe$var_info %>% filter(role == "predictor") %>% pull(variable)
      self$response <- recipe$term_info %>% filter(role == "outcome") %>% pull(variable)
    },
    
    train = function(valid_df) {
      train_df_juiced <- juice(prep(self$recipe))
      valid_df_juiced <- bake(prep(self$recipe), new_data = valid_df)
      
      x_form <- names(select(train_df_juiced, -self$response, -vin))
      
      # Fit a NB2 for starting values ---
      
      form <- reformulate(x_form, response = self$response)
      nb_fit <- glm.nb(form, data = train_df_juiced)
      betas_start <- tidy(nb_fit)$estimate
      phi_start <- log(1 / nb_fit$theta)
      
      # Fit a NB2 for starting values (naive model) ---
      
      form_naif <- reformulate("1", response = self$response)
      nb_fit_naif <- glm.nb(form_naif, data = train_df_juiced)
      betas_start_naif <- tidy(nb_fit_naif)$estimate
      phi_start_naif <- log(1 / nb_fit_naif$theta)
      
      # MBNV negative log-likelihood ---
      
      optim_fn <- function(x_mat) {
        mvnb_nll <- function(params) {
          data <- train_df_juiced
          
          data$mu <- exp(x_mat %*% params[1:(length(params) - 1)]) %>% as.vector()
          phi <- exp(params[length(params)])
          
          data <- 
            data %>%
            group_by(vin) %>%
            mutate(
              mu_bullet = cumsum(mu) - mu,
              y_bullet = cumsum(nb_claims) - nb_claims
            ) %>%
            ungroup()
          
          alpha <- phi + data$y_bullet
          gamma <- phi + data$mu_bullet
          
          c1 <- lgamma(data$nb_claims + alpha)
          c2 <- lgamma(alpha)
          c3 <- lgamma(data$nb_claims + 1)
          c4 <- data$nb_claims * log(data$mu / (data$mu + gamma))
          c5 <- alpha * log(gamma / (gamma + data$mu))
          
          ll <- c1 - c2 - c3 + c4 + c5
          
          - mean(ll)
        }
      }
      
      # Fit MVNB ---
      
      x_train <- train_df_juiced %>% select(-nb_claims, -vin) %>% bind_cols(Intercept = 1, .) %>% as.matrix()
      opt <- optim(c(betas_start, phi_start), fn = optim_fn(x_train), control = list(maxit = 20000), method = c("L-BFGS-B"))
      betas <- opt$par[seq_len(ncol(x_train))]
      phi <- as.numeric(exp(opt$par[ncol(x_train) + 1]))
      
      # Fit MVNB (naive) ---
      
      x_train_naif <- train_df_juiced %>% bind_cols(Intercept = 1, .) %>% select(Intercept) %>% as.matrix()
      opt_naif <- optim(c(betas_start_naif, phi_start_naif), fn = optim_fn(x_train_naif), control = list(maxit = 20000), method = c("L-BFGS-B"))
      betas_naif <- opt_naif$par[seq_len(ncol(x_train_naif))]
      phi_naif <- exp(opt_naif$par[ncol(x_train_naif) + 1])
      
      # Obtain mu, alpha and gamma vectors on valid set ---
      
      x_valid <- valid_df_juiced %>% select(-nb_claims, -vin) %>% bind_cols(Intercept = 1, .) %>% as.matrix()
      valid_mu <- exp(x_valid %*% betas) %>% as.vector()
      
      x_valid_naif <- valid_df_juiced %>% bind_cols(Intercept = 1, .) %>% select(Intercept) %>% as.matrix()
      valid_mu_naif <- exp(x_valid_naif %*% betas_naif) %>% as.vector()
      
      res_valid <-
        valid_df %>%
        mutate(
          mu = valid_mu,
          mu_naif = valid_mu_naif
        ) %>% 
        group_by(vin) %>%
        mutate(
          mu_bullet = cumsum(mu) - mu,
          mu_bullet_naif = cumsum(mu_naif) - mu_naif,
          y_bullet = cumsum(nb_claims) - nb_claims
        ) %>%
        ungroup() %>%
        mutate(
          alpha = phi + y_bullet,
          gamma = phi + mu_bullet,
          alpha_naif = phi_naif + y_bullet,
          gamma_naif = phi_naif + mu_bullet_naif
        ) %>% 
        mutate(
          prime = mu * alpha / gamma,
          prime_naif = mu_naif * alpha_naif / gamma_naif
        )
      
      # Assign attributes ---
      
      self$betas <- betas
      self$phi <- phi
      
      self$betas_naif <- betas_naif
      self$phi_naif <- phi_naif
      
      self$valid_mu <- valid_mu
      self$valid_alpha <- res_valid$alpha
      self$valid_gamma <- res_valid$gamma
      
      self$valid_mu_naif <- valid_mu_naif
      self$valid_alpha_naif <- res_valid$alpha_naif
      self$valid_gamma_naif <- res_valid$gamma_naif
      
      self$valid_prime <- res_valid$prime
      self$valid_prime_naif <- res_valid$prime_naif
      
      self$train_targets <- train_df_juiced[[self$response]]
      self$valid_targets <- valid_df_juiced[[self$response]]
      
      invisible(self)
    },
    
    print = function() {
      cat("Régression MVNB \n\n")
      cat("\tTraining set: ", length(self$train_targets), " observations\n", sep = "")
      cat("\tValidation set: ", length(self$valid_targets), " observations\n\n", sep = "")
      cat("\tVariable réponse: ", self$response, "\n\n", sep = "")
      cat("\tPrédicteurs:\n", glue("{self$predictors}\n"), sep = "\n\t")
      invisible(self)
    }
  )
)
