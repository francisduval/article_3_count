PoissonMLP <- 
  R6Class(
    classname = "PoissonMLP",
    inherit = NNUtils,
    
    public =
      list(
        spec = NULL,
        dataset = NULL,
        train_df = NULL,
        valid_df = NULL,
        
        train_targets = NULL,
        valid_targets = NULL,
        train_risk_vec = NULL,
        valid_risk_vec = NULL,
        valid_preds = NULL,
        
        initialize = function(spec, dataset, train, valid) {
          self$spec <- spec
          self$dataset <- dataset
          self$train_df <- train
          self$valid_df <- valid
        },
        
        train = function(epochs, lr_start, factor, patience, ...) {
          args <- list(...)
          
          train_ds <- self$dataset(self$train_df)
          valid_ds <- self$dataset(self$valid_df)
          
          self$train_targets <- as.numeric(train_ds$y)
          self$valid_targets <- as.numeric(valid_ds$y)
          
          train_dl <- dataloader(train_ds, batch_size = 256, shuffle = F)
          valid_dl <- dataloader(valid_ds, batch_size = 256, shuffle = F)
          
          model <- self$spec(...)
          optimiseur <- optim_adam(model$parameters, lr = lr_start)

          scheduler <- lr_reduce_on_plateau(optimiseur, factor = factor, patience = patience)
          
          train_risk_vec <- vector(mode = "numeric", length = epochs)
          valid_risk_vec <- vector(mode = "numeric", length = epochs)
          
          train_batch = function(b) {
            optimiseur$zero_grad()
            output <- model(b[[1]])
            target <- b[[2]]
            
            loss <- nnf_poisson_nll_loss(output, target, log_input = F)
            loss$backward()
            optimiseur$step()
            
            loss$item()
          }
          
          valid_batch = function(b) {
            output <- model(b[[1]])
            target <- b[[2]]
            
            loss <- nnf_poisson_nll_loss(output, target, log_input = F)
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
              best_model <- model
            } else if (valid_risk < valid_risk_vec[e - 1]) {
              best_model <- model
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
          
          self$valid_preds <- as.double(best_model$forward(valid_ds[1:length(valid_ds)]$x))
        }
      )
  )
