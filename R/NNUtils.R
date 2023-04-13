NNUtils <- R6Class(
  classname = "NNUtils",
  inherit = CountMetrics,
  
  public = 
    list(
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
