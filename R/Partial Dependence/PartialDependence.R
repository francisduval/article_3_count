PartialDependence <- 
  R6Class(
    "PartialDependence",
  
    public = list(
      model = NULL,
      predict_fn = NULL,
      data = NULL,
      pd_plots = NULL,
      
      initialize = function(model, predict_fn, data) {
        self$model <- model
        self$predict_fn <- predict_fn
        self$data <- data
        self$pd_plots <- list()
      },
      
      add_variable = function(variable) {
        pd_data <- self$compute_pd(variable)
        pd_plot <- self$plot_pd(pd_data, variable)
        self$pd_plots[[variable]] <- pd_plot
      },
      
      compute_pd = function(variable) {
        x_values <- seq(from = min(self$data[[variable]]), to = as.numeric(quantile(self$data[[variable]], p = 0.95)), length.out = 50)
        pd_data <- 
          matrix(NA, nrow = nrow(self$data), ncol = length(x_values)) %>% 
          set_colnames(x_values) %>% 
          as_tibble()
        
        for (i in 1:length(x_values)) {
          data <- self$data
          data[variable] <- x_values[i]
          pd_data[ , i] <- self$predict_fn(self$model, data)
        }
        
        return(pd_data)
      },
      
      plot_pd = function(pd_data, variable) {
        dat_plot <-
          pd_data %>% 
          pivot_longer(cols = everything()) %>% 
          mutate(name = as.numeric(name)) %>% 
          group_by(name) %>% 
          summarise(mean = mean(value))
        
        plot <- 
          ggplot(dat_plot, aes(x = name, y = mean)) +
          geom_line() +
          geom_point(aes(x = .data[[variable]], y = min(dat_plot$mean)), data = self$data, alpha = 0) +
          ylab(NULL) +
          xlab(variable) +
          scale_x_continuous(limits = c(min(self$data[[variable]]), quantile(self$data[[variable]], p = 0.95))) +
          theme(text = element_text(size = 15))
          
        
        ggMarginal(plot, type = "histogram", margins = "x", fill = "black", col = "white", bins = 80)
      },
      
      plot_all_pd = function(ncol = 2) {
        text_gpar <- gpar(fontsize = 15)
        grid.arrange(grobs = self$pd_plots, ncol = ncol, left = textGrob("Average expected number of claims", gp = text_gpar))
      }
    )
)
