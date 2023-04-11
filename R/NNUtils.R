NNUtils <- R6Class(
  classname = "NNUtils",
  inherit = CountMetrics,
  
  public = 
    list(
      plot_training = function() {
        self$training %>%
          mutate(epochs = seq_along(train_loss)) %>%
          pivot_longer(cols = -"epochs") %>%
          separate(col = "name", into = c("dataset", "metric"), sep = "_") %>%
          ggplot(aes(x = epochs, y = value, col = dataset)) +
          geom_point(shape = 21) +
          geom_line() +
          scale_color_discrete(name = NULL) +
          ylab(NULL) +
          facet_grid(vars(metric), scales = "free_y")
      }
    )
)
