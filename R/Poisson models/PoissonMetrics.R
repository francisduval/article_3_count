PoissonMetrics <- R6Class(
  classname = "PoissonMetrics",
  
  public = 
    list(
      pred_naif_vec = function() rep(mean(self$train_targets), length(self$valid_targets)),
      sum_p_2 = function(mu) sum(map_dbl(0:20, ~ dpois(., lambda = mu) ^ 2)),
      prob_vec_naif = function() dpois(self$valid_targets, self$pred_naif_vec()),
      prob_vec = function() dpois(self$valid_targets, self$valid_mu),
      dev_one_obs = function(mu, target) {
        x1 <- if_else(target == 0, 0, target * (log(target) - log(mu)))
        x2 <- target - mu
        2 * (x1 - x2)
      },
      rps_one_obs = function(mu, target) {
        p_k <- map_dbl(0:40, ~ ppois(., lambda = mu))
        ind_target_le_k <- map_dbl(0:40, ~ as.numeric(target <= .))
        sum((p_k - ind_target_le_k) ^ 2)
      },
      
      # ---
      
      dev_naif = function() mean(map2_dbl(self$pred_naif_vec(), self$valid_targets, ~ dev_one_obs(.x, .y))),
      logs_naif = function() mean(-log(self$prob_vec_naif())),
      ses_naif = function() mean(ses(self$valid_targets, self$pred_naif_vec())),
      dss_naif = function() mean(((self$valid_targets - self$pred_naif_vec()) / self$pred_naif_vec()) ^ 2 + 2 * log(self$pred_naif_vec())),
      qs_naif = function() {
        norm_p_vec <- map_dbl(self$pred_naif_vec(), self$sum_p_2)
        score_vec <- -2 * self$prob_vec_naif() + norm_p_vec
        mean(score_vec)
      },
      sphs_naif = function() {
        norm_p_vec <- map_dbl(self$pred_naif_vec(), self$sum_p_2)
        score_vec <- -self$prob_vec_naif() / norm_p_vec
        mean(score_vec)
      },
      rps_naif = function() {
        mean(map2_dbl(self$prob_vec_naif(), self$valid_targets, ~ rps_one_obs(.x, .y)))
      },
      
      # ---
      
      dev = function() mean(map2_dbl(self$valid_mu, self$valid_targets, ~ dev_one_obs(.x, .y))),
      logs = function() mean(-log(self$prob_vec())),
      ses = function() mean(ses(self$valid_targets, self$valid_mu)),
      dss = function() mean(((self$valid_targets - self$valid_mu) / self$valid_mu) ^ 2 + 2 * log(self$valid_mu)),
      qs = function() {
        norm_p_vec <- map_dbl(self$valid_mu, self$sum_p_2)
        score_vec <- -2 * self$prob_vec() + norm_p_vec
        mean(score_vec)
      },
      sphs = function() {
        norm_p_vec <- map_dbl(self$valid_mu, self$sum_p_2)
        score_vec <- -self$prob_vec() / norm_p_vec
        mean(score_vec)
      },
      rps = function() {
        mean(map2_dbl(self$valid_mu, self$valid_targets, ~ rps_one_obs(.x, .y)))
      },
      
      # ---
      
      dev_skill = function() 1 - self$dev() / self$dev_naif(),
      logs_skill = function() 1 - self$logs() / self$logs_naif(),
      ses_skill = function() 1 - self$ses() / self$ses_naif(),
      dss_skill = function() 1 - self$dss() / self$dss_naif(),
      qs_skill = function() -(1 - self$qs() / self$qs_naif()),
      sphs_skill = function() -(1 - self$sphs() / self$sphs_naif()),
      rps_skill = function() 1 - self$rps() / self$rps_naif(),
      
      # ---
      
      print_metrics = function() {
        cat("-----------------------------------------------\n")
        cat("Modèle naïf\n")
        cat("-----------------------------------------------\n")
        cat("Poisson deviance = ", round(self$dev_naif(), 5), "\n")
        cat("Logarithmic score = ", round(self$logs_naif(), 5), "\n")
        cat("Squared error = ", round(self$ses_naif(), 5), "\n")
        cat("Dawid-Sebastiani = ", round(self$dss_naif(), 5), "\n")
        cat("Quadratic score = ", round(self$qs_naif(), 5), "\n")
        cat("Spherical = ", round(self$sphs_naif(), 5), "\n")
        cat("Ranked probability = ", round(self$rps_naif(), 5), "\n\n")
        cat("-----------------------------------------------\n")
        cat("Performance absolue\n")
        cat("-----------------------------------------------\n")
        cat("Poisson deviance = ", round(self$dev(), 5), "\n")
        cat("Logarithmic score = ", round(self$logs(), 5), "\n")
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
        cat("Squared error = ", scales::percent(self$ses_skill(), accuracy = 0.01), "\n")
        cat("Dawid-Sebastiani = ", scales::percent(self$dss_skill(), accuracy = 0.01), "\n")
        cat("Quadratic score = ", scales::percent(self$qs_skill(), accuracy = 0.01), "\n")
        cat("Spherical = ", scales::percent(self$sphs_skill(), accuracy = 0.01), "\n")
        cat("Ranked probability = ", scales::percent(self$rps_skill(), accuracy = 0.01), "\n")
      }
    )
)
