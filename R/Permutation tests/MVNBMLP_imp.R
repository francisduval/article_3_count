MVNBMLP_imp <- 
  R6Class(
    classname = "MVNBMLP_imp",
    
    public =
      list(
        spec = NULL,
        dataset = NULL,
        
        train_targets = NULL,
        valid_targets = NULL,
        train_risk_vec = NULL,
        valid_risk_vec = NULL,
        
        trained_model = NULL,
        
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
            phi <- as.double(model$phi())
            alpha <- as.double(model$alpha(valid_ds[1:length(valid_ds)]$x))
            gamma <- as.double(model$gamma(valid_ds[1:length(valid_ds)]$x))
            
            if (e == 1) {
              best_model <- model
              best_valid_mu <- mu
              best_train_mu <- train_mu
              best_phi <- phi
              best_valid_alpha <- alpha
              best_valid_gamma <- gamma
              best_valid_loss <- mvnb_loss(mu, alpha, gamma, self$valid_targets)
            } else if (valid_risk < as.numeric(best_valid_loss)) {
              best_model <- model
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
          
          self$trained_model <- best_model
        }
      )
  )