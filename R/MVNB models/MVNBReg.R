MVNBReg <- R6Class(
  classname = "MVNBReg",
  inherit = CountMetrics,
  
  public = list(
    recipe = NULL,
    predictors = NULL,
    response = NULL,

    train_targets = NULL,
    valid_targets = NULL,

    valid_res = NULL,
    
    betas = NULL,
    
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
      
      # Useful functions ---
      
      sum_p_2_pois <- function(mu) sum(map_dbl(0:30, ~ dpois(., lambda = mu) ^ 2))
      sum_p_2_mvnb <- function(mu, alpha, gamma) sum(map_dbl(0:30, ~ dnb2gen(., mu = mu, alpha = alpha, gamma = gamma) ^ 2))
      
      # Assign attributes ---
      
      x_valid <- valid_df_juiced %>% select(-nb_claims, -vin) %>% bind_cols(Intercept = 1, .) %>% as.matrix()
      valid_mu <- exp(x_valid %*% betas) %>% as.vector()
      
      self$train_targets <- train_df_juiced[[self$response]]
      self$valid_targets <- valid_df_juiced[[self$response]]
      
      self$valid_res <-
        valid_df %>%
        mutate(mu = valid_mu) %>% 
        group_by(vin) %>%
        mutate(
          mu_bullet = cumsum(mu) - mu,
          y_bullet = cumsum(nb_claims) - nb_claims
        ) %>%
        ungroup() %>%
        select(vin, contract_start_date, nb_claims, mu_bullet, y_bullet, mu) %>% 
        mutate(
          alpha = phi + y_bullet,
          gamma = phi + mu_bullet,
          phi = phi,
          mean = mu * alpha / gamma,
          sd = sqrt(mu * alpha * (gamma + mu) * gamma ^ (-2)),
          prob = dnb2gen(self$valid_targets, mu = mu, alpha = alpha, gamma = gamma),
          norm_carre_p = pmap_dbl(list(mu, alpha, gamma), ~ sum_p_2_mvnb(mu = ..1, alpha = ..2, gamma = ..3)),
          
          pred_naif = rep(mean(self$train_targets), length(self$valid_targets)),
          prob_naif = dpois(self$valid_targets, pred_naif),
          norm_carre_p_naif = map_dbl(pred_naif, sum_p_2_pois)
        )
      
      self$betas <- betas
      
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
