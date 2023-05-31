PoissonMLP <- 
  R6Class(
    classname = "PoissonMLP",
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
        
        initialize = function(spec, dataset) {
          self$spec <- spec
          self$dataset <- dataset
        },
        
        train = function(train, valid, epochs, lr_start, factor, patience, batch = 64, ...) {
          train_ds <- self$dataset(train)
          valid_ds <- self$dataset(valid)
          
          self$train_targets <- as.numeric(train_ds$y)
          self$valid_targets <- as.numeric(valid_ds$y)
          
          train_dl <- dataloader(train_ds, batch_size = batch, shuffle = F)
          valid_dl <- dataloader(valid_ds, batch_size = batch, shuffle = F)
          
          model <- self$spec(...)
          optimiseur <- optim_adam(model$parameters, lr = lr_start)
          
          scheduler <- lr_reduce_on_plateau(optimiseur, factor = factor, patience = patience)
          
          train_risk_vec <- vector(mode = "numeric", length = epochs)
          valid_risk_vec <- vector(mode = "numeric", length = epochs)
          
          train_batch = function(b) {
            optimiseur$zero_grad()
            output <- model(b[[1]])
            target <- b[[2]]
            
            loss <- nnf_poisson_nll_loss(output, target, log_input = F, full = T)
            loss$backward()
            optimiseur$step()
            
            loss$item()
          }
          
          valid_batch = function(b) {
            output <- model(b[[1]])
            target <- b[[2]]
            
            loss <- nnf_poisson_nll_loss(output, target, log_input = F, full = T)
            loss$item()
          }
          
          # -----
          
          for (e in 1:epochs) {
            
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
            
            mu <- as.double(model$forward(valid_ds[1:length(valid_ds)]$x))
            train_mu <- as.double(model$forward(train_ds[1:length(train_ds)]$x))
            
            if (e == 1) {
              best_valid_mu <- mu
              best_train_mu <- train_mu
              best_valid_loss <- as.numeric(nnf_poisson_nll_loss(best_valid_mu, self$valid_targets, log_input = F, full = T))
            } else if (valid_risk < best_valid_loss) {
              best_valid_mu <- mu
              best_train_mu <- train_mu
              best_valid_loss <- as.numeric(nnf_poisson_nll_loss(best_valid_mu, self$valid_targets, log_input = F, full = T))
            }
            
            train_risk_vec[e] <- train_risk
            valid_risk_vec[e] <- valid_risk
            
            lr <- optimiseur$param_groups[[1]]$lr
            scheduler$step(valid_risk)
            
            cat(sprintf(
              "\nEpoch %d (lr = %g), training loss: %3.4f, validation loss: %3.4f",
              e, lr, train_risk, valid_risk
            ))
          }
          
          # -----
          
          self$train_risk_vec <- train_risk_vec
          self$valid_risk_vec <- valid_risk_vec
          
          sum_p_2 <- function(mu) sum(map_dbl(0:30, ~ dpois(., lambda = mu) ^ 2))
          
          self$valid_res <- 
            valid %>% 
            select(vin, contract_start_date, nb_claims) %>% 
            mutate(
              mu = best_valid_mu,
              mean = mu,
              sd = sqrt(mu),
              prob = dpois(self$valid_targets, mu),
              norm_carre_p = map_dbl(mu, sum_p_2),
              
              pred_naif = rep(mean(self$train_targets), length(self$valid_targets)),
              prob_naif = dpois(self$valid_targets, pred_naif),
              norm_carre_p_naif = map_dbl(pred_naif, sum_p_2)
            )
          
          self$train_res <- 
            train %>% 
            select(vin, contract_start_date, nb_claims) %>% 
            mutate(
              mu = best_train_mu,
              mean = mu,
              sd = sqrt(mu),
              prob = dpois(self$train_targets, mu),
              norm_carre_p = map_dbl(mu, sum_p_2)
            )
        },
        
        plot_training = function() {
          tibble(
            epoch = seq_along(self$train_risk_vec),
            train_loss = self$train_risk_vec,
            valid_loss = self$valid_risk_vec,
          ) %>% 
            pivot_longer(cols = -"epoch") %>% 
            ggplot(aes(x = epoch, y = value, col = name)) +
            geom_point(shape = 21) +
            geom_line() +
            scale_color_discrete(name = NULL) +
            scale_x_continuous(breaks = seq_along(self$train_risk_vec)) +
            ylab(NULL)
        }
      )
  )
