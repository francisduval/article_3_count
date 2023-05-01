NB2MLP <- 
  R6Class(
    classname = "NB2MLP",
    inherit = NB2Metrics,
    
    public =
      list(
        spec = NULL,
        dataset = NULL,
        
        train_targets = NULL,
        valid_targets = NULL,
        train_risk_vec = NULL,
        valid_risk_vec = NULL,
        valid_mu = NULL,
        phi = NULL,
        
        valid_mu_naif = NULL,
        phi_naif = NULL,
        
        initialize = function(spec, dataset) {
          self$spec <- spec
          self$dataset <- dataset
        },
        
        fit_naive_model = function(train_df, valid_df) {
          fit <- glm.nb(nb_claims ~ 1, data = train_df)
          self$valid_mu_naif <- as.numeric(predict(fit, type = "response", newdata = valid_df))[1]
          self$phi_naif <- 1 / fit$theta
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
        },
        
        train = function(train, valid, epochs, lr_start, factor, patience, batch = 256, ...) {
          self$fit_naive_model(train, valid)
          
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
            
            mu <- output$mu
            phi <- output$phi
            
            loss <- nb2_loss(mu, phi, target)
            loss$backward()
            optimiseur$step()
            
            loss$item()
          }
          
          valid_batch = function(b) {
            output <- model(b[[1]])
            target <- b[[2]]
            
            mu <- output$mu
            phi <- output$phi
            
            loss <- nb2_loss(mu, phi, target)
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
            
            if (e == 1) {
              self$valid_mu <- as.double(model$forward(valid_ds[1:length(valid_ds)]$x)$mu)
              self$phi <- as.double(model$forward(valid_ds[1:length(valid_ds)]$x)$phi)
              best_valid_loss <- nb2_loss(self$valid_mu, self$phi, self$valid_targets)
            } else if (valid_risk < as.numeric(best_valid_loss)) {
              self$valid_mu <- as.double(model$forward(valid_ds[1:length(valid_ds)]$x)$mu)
              self$phi <- as.double(model$forward(valid_ds[1:length(valid_ds)]$x)$phi)
              best_valid_loss <- nb2_loss(self$valid_mu, self$phi, self$valid_targets)
            }
            
            train_risk_vec[e] <- train_risk
            valid_risk_vec[e] <- valid_risk
            
            lr <- optimiseur$param_groups[[1]]$lr
            scheduler$step(valid_risk)
            
            cat(sprintf(
              "\nEpoch %d (lr = %g), training loss: %3.4f, validation loss: %3.4f, phi = %3.4f",
              e, lr, train_risk, valid_risk, self$phi
            ))
          }
          
          # -----
          
          self$train_risk_vec <- train_risk_vec
          self$valid_risk_vec <- valid_risk_vec
          
        }
      )
  )
