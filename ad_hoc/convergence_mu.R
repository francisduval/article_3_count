train_mvnb <- tar_read(train_mvnb)[1:1000, ]
valid_mvnb <- tar_read(valid_mvnb)[1:250, ]

MVNBMLP <- 
  R6Class(
    classname = "MVNBMLP",
    inherit = CountMetrics,
    
    public =
      list(
        spec = NULL,
        dataset = NULL,
        
        train_targets = NULL,
        valid_targets = NULL,
        train_risk_vec = NULL,
        valid_risk_vec = NULL,
        
        train_res = NULL,
        valid_res = NULL,
        
        mu_list = NULL,
        
        initialize = function(spec, dataset) {
          self$spec <- spec
          self$dataset <- dataset
        },
        
        train = function(train, valid, epochs, lr_start, factor, patience, batch = 256, ...) {
          model <- self$spec(...)
          optimiseur <- optim_adam(model$parameters, lr = lr_start)
          scheduler <- lr_reduce_on_plateau(optimiseur, factor = factor, patience = patience)
          
          train_risk_vec <- vector(mode = "numeric", length = epochs)
          valid_risk_vec <- vector(mode = "numeric", length = epochs)
          
          # -----
          
          train_batch = function(b) {
            optimiseur$zero_grad()
            output <- model(b[[1]])
            target <- b[[2]]
            
            mu <- output$mu
            alpha <- output$alpha
            gamma <- output$gamma
            
            loss <- mvnb_loss(mu, alpha, gamma, target)
            loss$backward()
            optimiseur$step()
            
            loss$item()
          }
          
          valid_batch = function(b) {
            output <- model(b[[1]])
            target <- b[[2]]
            
            mu <- output$mu
            alpha <- output$alpha
            gamma <- output$gamma
            
            loss <- mvnb_loss(mu, alpha, gamma, target)
            loss$item()
          }
          
          # -----
          
          mu_list <- list()
          
          for (e in 1:epochs) {
            
            if (e > 1) {
              mu_tibble <- tibble(mu_before = train$mu)
              
              train$mu <- as.double(model$mu(train_ds[1:length(train_ds)]$x))
              train <- compute_sum_past_mu(train)
              
              valid$mu <- as.double(model$mu(valid_ds[1:length(valid_ds)]$x))
              valid <- compute_sum_past_mu(valid)
              
              mu_tibble <- mutate(mu_tibble, mu_after = train$mu)
              mu_list <- c(mu_list, list(mu_tibble))
            }
            
            train_ds <- self$dataset(train)
            valid_ds <- self$dataset(valid)
            
            train_dl <- dataloader(train_ds, batch_size = batch, shuffle = F)
            valid_dl <- dataloader(valid_ds, batch_size = batch, shuffle = F)
            
            self$train_targets <- as.numeric(train_ds$y)
            self$valid_targets <- as.numeric(valid_ds$y)
            
            model$train()
            train_loss_vec <- c()
            
            coro::loop(for (b in train_dl) {
              loss <- train_batch(b)
              train_loss_vec <- c(train_loss_vec, loss)
            })
            
            model$eval()
            valid_loss_vec <- c()
            
            with_no_grad({
              coro::loop(for (b in valid_dl) {
                loss <- valid_batch(b)
                valid_loss_vec <- c(valid_loss_vec, loss)
              })
            })
            
            train_risk <- mean(train_loss_vec)
            valid_risk <- mean(valid_loss_vec)
            
            mu <- as.double(model$mu(valid_ds[1:length(valid_ds)]$x))
            train_mu <- as.double(model$mu(train_ds[1:length(train_ds)]$x))
            phi <- as.double(model$phi(valid_ds[1:length(valid_ds)]$x))
            alpha <- as.double(model$alpha(valid_ds[1:length(valid_ds)]$x))
            gamma <- as.double(model$gamma(valid_ds[1:length(valid_ds)]$x))
            
            if (e == 1) {
              best_valid_mu <- mu
              best_train_mu <- train_mu
              best_phi <- phi
              best_valid_alpha <- alpha
              best_valid_gamma <- gamma
              best_valid_loss <- mvnb_loss(mu, alpha, gamma, self$valid_targets)
            } else if (valid_risk < as.numeric(best_valid_loss)) {
              best_valid_mu <- mu
              best_train_mu <- train_mu
              best_phi <- phi
              best_valid_alpha <- alpha
              best_valid_gamma <- gamma
              best_valid_loss <- mvnb_loss(mu, alpha, gamma, self$valid_targets)
            }
            
            train_risk_vec[e] <- train_risk
            valid_risk_vec[e] <- valid_risk
            
            lr <- optimiseur$param_groups[[1]]$lr
            scheduler$step(valid_risk)
            
            cat(sprintf(
              "\nEpoch %d (lr = %g), training loss: %3.4f, validation loss: %3.4f, phi: %3.4f",
              e, lr, train_risk, valid_risk, phi
            ))
          }
          
          # -----
          
          self$train_risk_vec <- train_risk_vec
          self$valid_risk_vec <- valid_risk_vec
          
          sum_p_2_pois <- function(mu) sum(map_dbl(0:30, ~ dpois(., lambda = mu) ^ 2))
          sum_p_2_mvnb <- function(mu, alpha, gamma) sum(map_dbl(0:30, ~ dnb2gen(., mu = mu, alpha = alpha, gamma = gamma) ^ 2))
          
          self$valid_res <-
            valid %>%
            mutate(mu = best_valid_mu) %>% 
            group_by(vin) %>%
            mutate(
              mu_bullet = cumsum(mu) - mu,
              y_bullet = cumsum(nb_claims) - nb_claims
            ) %>%
            ungroup() %>%
            select(vin, contract_start_date, nb_claims, mu_bullet, y_bullet, mu) %>% 
            mutate(
              alpha = best_phi + y_bullet,
              gamma = best_phi + mu_bullet,
              phi = best_phi,
              mean = mu * alpha / gamma,
              sd = sqrt(mu * alpha * (gamma + mu) * gamma ^ (-2)),
              prob = dnb2gen(self$valid_targets, mu = mu, alpha = alpha, gamma = gamma),
              norm_carre_p = pmap_dbl(list(mu, alpha, gamma), ~ sum_p_2_mvnb(mu = ..1, alpha = ..2, gamma = ..3)),
              
              pred_naif = rep(mean(self$train_targets), length(self$valid_targets)),
              prob_naif = dpois(self$valid_targets, pred_naif),
              norm_carre_p_naif = map_dbl(pred_naif, sum_p_2_pois)
            )
          
          self$train_res <-
            train %>%
            mutate(mu = best_train_mu) %>% 
            group_by(vin) %>%
            mutate(
              mu_bullet = cumsum(mu) - mu,
              y_bullet = cumsum(nb_claims) - nb_claims
            ) %>%
            ungroup() %>%
            select(vin, contract_start_date, nb_claims, mu_bullet, y_bullet, mu) %>% 
            mutate(
              alpha = best_phi + y_bullet,
              gamma = best_phi + mu_bullet,
              phi = best_phi,
              mean = mu * alpha / gamma,
              sd = sqrt(mu * alpha * (gamma + mu) * gamma ^ (-2)),
              prob = dnb2gen(self$train_targets, mu = mu, alpha = alpha, gamma = gamma),
              norm_carre_p = pmap_dbl(list(mu, alpha, gamma), ~ sum_p_2_mvnb(mu = ..1, alpha = ..2, gamma = ..3))
            )
          
          self$mu_list <- mu_list
        },
        
        plot_training = function() {
          tibble(
            epoch = seq_along(self$train_risk_vec),
            train_loss = self$train_risk_vec,
            valid_loss = self$valid_risk_vec,
          ) %>% 
            pivot_longer(cols = -"epoch") %>% 
            ggplot(aes(x = epoch, y = value, col = name)) +
            geom_point(alpha = 0.7) +
            geom_line(linewidth = 0.8, alpha = 0.3) +
            scale_color_discrete(name = NULL) +
            scale_x_continuous(breaks = seq_along(self$train_risk_vec)) +
            ylab(NULL)
        }
      )
  )


model <- MVNBMLP$new(MVNBCANN3L, DatasetNNMVNB)
model$train(train_mvnb, valid_mvnb, epochs = 10, lr_start = 0.00001, factor = 0.3, patience = 2, batch = 256, p = 0.3, n_1L = 128, n_2L = 64, n_3L = 32)

combined_df <- bind_rows(model$mu_list, .id = "epoch")


# Function to calculate R-squared
calculate_r2 <- function(df) {
  lm_model <- lm(mu_after ~ mu_before, data = df)
  r_squared <- summary(lm_model)$r.squared
  return(r_squared)
}

# Calculate R-squared for each tibble
r_squared_values <- map_dbl(model$mu_list, calculate_r2)

# Add R-squared as a column in combined_df
combined_df$r_squared <- rep(r_squared_values, each = nrow(train_mvnb))

ggplot(combined_df, aes(x = mu_before, y = mu_after)) +
  geom_point(alpha = 0.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Convergence des mus", x = "Mu before epoch", y = "Mu after epoch") +
  facet_wrap(vars(epoch), nrow = 3) +
  geom_text(
    mapping = aes(x = 0.15, y = 0.4, label = paste("R² =", round(r_squared, 2))), 
    hjust = 1, 
    vjust = 1, 
    size = 3
  )
