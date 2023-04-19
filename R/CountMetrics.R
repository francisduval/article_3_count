CountMetrics <- R6Class(
  classname = "CountMetrics",
  
  public = 
    list(
      pred_naif_vec = function() rep(mean(self$train_targets), length(self$valid_targets)),
      
      mse_naif = function() mean(ses(self$valid_targets, self$pred_naif_vec())),
      poisson_logloss_naif = function() as.numeric(nnf_poisson_nll_loss(self$pred_naif_vec(), self$valid_targets, log_input = F)),
      logscore_naif = function() mean(logs(self$valid_targets, self$pred_naif_vec())),
        
      mse = function() mean(ses(self$valid_targets, self$valid_preds)),
      poisson_logloss = function() as.numeric(nnf_poisson_nll_loss(self$valid_preds, self$valid_targets, log_input = F)),
      logscore = function() mean(logs(self$valid_targets, self$valid_preds)),
      
      mse_skill = function() 1 - self$mse() / self$mse_naif(),
      poisson_logloss_skill = function() 1 - self$poisson_logloss() / self$poisson_logloss_naif(),
      logscore_skill = function() 1 - self$logscore() / self$logscore_naif(),
      
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
