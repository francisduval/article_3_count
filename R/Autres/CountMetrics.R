CountMetrics <- R6Class(
  classname = "CountMetrics",
  
  public = 
    list(
      dev_one_obs = function(pred, target) {
        x1 <- if_else(target == 0, 0, target * (log(target) - log(pred)))
        x2 <- target - pred
        2 * (x1 - x2)
      },
      
      rps_one_obs_pois = function(mu, target) {
        p_k <- map_dbl(0:30, ~ ppois(., lambda = mu))
        ind_target_le_k <- map_dbl(0:30, ~ as.numeric(target <= .))
        sum((p_k - ind_target_le_k) ^ 2)
      },
  
      # ---
  
      dev_naif = function() mean(map2_dbl(self$valid_res$pred_naif, self$valid_targets, ~ self$dev_one_obs(.x, .y))),
      logs_naif = function() mean(-log(self$valid_res$prob_naif)),
      aes_naif = function() mean(abs(self$valid_targets - self$valid_res$pred_naif)),
      ses_naif = function() mean(ses(self$valid_targets, self$valid_res$pred_naif)),
      dss_naif = function() mean(((self$valid_targets - self$valid_res$pred_naif) / sqrt(self$valid_res$pred_naif)) ^ 2 + 2 * log(sqrt(self$valid_res$pred_naif))),
      qs_naif = function() mean(-2 * self$valid_res$prob_naif + self$valid_res$norm_carre_p_naif),
      sphs_naif = function() mean(-self$valid_res$prob_naif / sqrt(self$valid_res$norm_carre_p_naif)),
      rps_naif = function() mean(map2_dbl(self$valid_res$prob_naif, self$valid_targets, ~ self$rps_one_obs_pois(.x, .y))),
      
      # ---
      
      logs_train = function() mean(-log(self$train_res$prob)),
      
      dev = function() mean(map2_dbl(self$valid_res$mean, self$valid_targets, ~ self$dev_one_obs(.x, .y))),
      logs = function() mean(-log(self$valid_res$prob)),
      aes = function() mean(abs(self$valid_targets - self$valid_res$mean)),
      ses = function() mean(ses(self$valid_targets, self$valid_res$mean)),
      dss = function() mean(((self$valid_targets - self$valid_res$mean) / self$valid_res$sd) ^ 2 + 2 * log(self$valid_res$sd)),
      qs = function() mean(-2 * self$valid_res$prob + self$valid_res$norm_carre_p),
      sphs = function() mean(-self$valid_res$prob / sqrt(self$valid_res$norm_carre_p)),
      rps = function() mean(map2_dbl(self$valid_res$prob, self$valid_targets, ~ self$rps_one_obs_pois(.x, .y))),
      
      # ---
      
      dev_skill = function() 1 - self$dev() / self$dev_naif(),
      logs_skill = function() 1 - self$logs() / self$logs_naif(),
      aes_skill = function() 1 - self$aes() / self$aes_naif(),
      ses_skill = function() 1 - self$ses() / self$ses_naif(),
      dss_skill = function() 1 - self$dss() / self$dss_naif(),
      qs_skill = function() - (1 - self$qs() / self$qs_naif()),
      sphs_skill = function() - (1 - self$sphs() / self$sphs_naif()),
      rps_skill = function() 1 - self$rps() / self$rps_naif(),
      
      # ---
      
      print_metrics = function() {
        cat("-----------------------------------------------\n")
        cat("Training set logarithmic score\n")
        cat("-----------------------------------------------\n")
        cat("Logarithmic score = ", round(self$logs_train(), 5), "\n\n")
        cat("-----------------------------------------------\n")
        cat("Modèle naïf\n")
        cat("-----------------------------------------------\n")
        cat("Poisson deviance = ", round(self$dev_naif(), 5), "\n")
        cat("Logarithmic score = ", round(self$logs_naif(), 5), "\n")
        cat("Squared error = ", round(self$ses_naif(), 5), "\n")
        cat("Absolute error = ", round(self$aes_naif(), 5), "\n")
        cat("Dawid-Sebastiani = ", round(self$dss_naif(), 5), "\n")
        cat("Quadratic score = ", round(self$qs_naif(), 5), "\n")
        cat("Spherical = ", round(self$sphs_naif(), 5), "\n")
        cat("Ranked probability = ", round(self$rps_naif(), 5), "\n\n")
        cat("-----------------------------------------------\n")
        cat("Performance absolue\n")
        cat("-----------------------------------------------\n")
        cat("Poisson deviance = ", round(self$dev(), 5), "\n")
        cat("Logarithmic score = ", round(self$logs(), 5), "\n")
        cat("Absolute error = ", round(self$aes(), 5), "\n")
        cat("Squared error = ", round(self$ses(), 5), "\n")
        cat("Dawid-Sebastiani = ", round(self$dss(), 5), "\n")
        cat("Quadratic score = ", round(self$qs(), 5), "\n")
        cat("Spherical = ", round(self$sphs(), 5), "\n")
        cat("Ranked probability = ", round(self$rps(), 5), "\n\n")
        cat("-----------------------------------------------\n")
        cat("Skill\n")
        cat("-----------------------------------------------\n")
        cat("Poisson deviance = ", scales::percent(self$dev_skill(), accuracy = 0.01), "\n")
        cat("Logarithmic score = ", scales::percent(self$logs_skill(), accuracy = 0.01), "\n")
        cat("Absolute error = ", scales::percent(self$aes_skill(), accuracy = 0.01), "\n")
        cat("Squared error = ", scales::percent(self$ses_skill(), accuracy = 0.01), "\n")
        cat("Dawid-Sebastiani = ", scales::percent(self$dss_skill(), accuracy = 0.01), "\n")
        cat("Quadratic score = ", scales::percent(self$qs_skill(), accuracy = 0.01), "\n")
        cat("Spherical = ", scales::percent(self$sphs_skill(), accuracy = 0.01), "\n")
        cat("Ranked probability = ", scales::percent(self$rps_skill(), accuracy = 0.01), "\n")
      }
    )
)