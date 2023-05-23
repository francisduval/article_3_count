MVNBMetrics <- R6Class(
  classname = "MVNBMetrics",
  
  public = 
    list(
      mse_naif = function() mean(ses(self$valid_targets, self$valid_prime_naif)),
      # logloss_naif = function() as.numeric(nb2_loss(self$mu_naif(), self$phi_naif, self$valid_targets)),
      logscore_naif = function() - mean(dnb2gen(self$valid_targets, mu = self$valid_mu_naif, alpha = self$valid_alpha_naif, gamma = self$valid_gamma_naif, ln = T)),
      
      mse = function() mean(ses(self$valid_targets, self$valid_prime)),
      # logloss = function() as.numeric(nb2_loss(self$valid_mu, self$phi, self$valid_targets)),
      logscore = function() - mean(dnb2gen(self$valid_targets, mu = self$valid_mu, alpha = self$valid_alpha, gamma = self$valid_gamma, ln = T)),
      
      mse_skill = function() 1 - self$mse() / self$mse_naif(),
      # logloss_skill = function() 1 - self$logloss() / self$logloss_naif(),
      logscore_skill = function() 1 - self$logscore() / self$logscore_naif(),
      
      print_metrics = function() {
        cat("Modèle naïf\n")
        cat("-----------------------------------------------\n")
        cat("MSE = ", round(self$mse_naif(), 4), "\n")
        # cat("NB2 log-loss = ", round(self$logloss_naif(), 4), "\n")
        cat("Logarithmic score = ", round(self$logscore_naif(), 4), "\n\n")
        cat("Performance absolue\n")
        cat("-----------------------------------------------\n")
        cat("MSE = ", round(self$mse(), 4), "\n")
        # cat("Nb2 log-loss = ", round(self$logloss(), 4), "\n")
        cat("Logarithmic score = ", round(self$logscore(), 4), "\n\n")
        cat("Skill\n")
        cat("-----------------------------------------------\n")
        cat("MSE = ", scales::percent(self$mse_skill(), accuracy = 0.01), "\n")
        # cat("Nb2 log-loss = ", scales::percent(self$logloss_skill(), accuracy = 0.01), "\n")
        cat("Logarithmic score = ", scales::percent(self$logscore_skill(), accuracy = 0.01), "\n")
      }
    )
)