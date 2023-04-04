CountMetrics <- R6Class(
  classname = "CountMetrics",
  
  public = 
    list(
      pred_naif_vec = function() rep(mean(c(self$train_targets, self$valid_targets)), length(self$test_targets)),
      
      mse_naif = function() mean((self$test_targets - self$pred_naif_vec()) ^ 2),
      poisson_logloss_naif = function() poisson_log_loss_vec(self$test_targets, self$pred_naif_vec()),
      logscore_naif = function() mean(log(abs(self$test_targets - self$pred_naif_vec()))),
        
      mse = function() mean((self$test_targets - self$test_preds) ^ 2),
      poisson_logloss = function() poisson_log_loss_vec(self$test_targets, self$test_preds),
      logscore = function() mean(log(abs(self$test_targets - self$test_preds))), 
      
      mse_skill = function() 1 - self$mse() / self$mse_naif(),
      poisson_logloss_skill = function() 1 - self$poisson_logloss() / self$poisson_logloss_naif(),
      logscore_skill = function() 1 - self$logscore_naif() / self$logscore() ,
      
      print_metrics = function() {
        cat("Modèle naïf\n")
        cat("-----------------------------------------------\n")
        cat("MSE = ", round(self$mse_naif(), 4), "\n")
        cat("Poisson log-loss = ", round(self$poisson_logloss_naif(), 4), "\n")
        cat("Logarithmic score = ", round(self$logscore_naif(), 4), "\n\n")
        cat("Performance absolue\n")
        cat("-----------------------------------------------\n")
        cat("MSE = ", round(self$mse(), 4), "\n")
        cat("Poisson log-loss = ", round(self$poisson_logloss(), 4), "\n")
        cat("Logarithmic score = ", round(self$logscore(), 4), "\n\n")
        cat("Skill\n")
        cat("-----------------------------------------------\n")
        cat("MSE = ", scales::percent(round(self$mse_skill(), 4)), "\n")
        cat("Poisson log-loss = ", scales::percent(round(self$poisson_logloss_skill(), 4)), "\n")
        cat("Logarithmic score = ", scales::percent(round(self$logscore_skill(), 4)), "\n")
      }
    )
)
