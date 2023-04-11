PoissonMLP1L <- 
  nn_module(
    "PoissonMLP1L",
    
    initialize = function(input_size_mlp, input_size_skip, nb_neurons = 32, beta_vec) {
      self$bn0 = nn_batch_norm1d(input_size_mlp)
      self$linear1 = nn_linear(input_size_mlp, nb_neurons)
      self$bn1 = nn_batch_norm1d(nb_neurons)
      self$linear2 = nn_linear(nb_neurons, 1)
      
      self$bn_skip = nn_batch_norm1d(input_size_skip)
      self$linear_skip = nn_linear(input_size_skip, 1)
      
      self$init_params(beta_vec, input_size_skip)
    },
    
    init_params = function(beta_vec, input_size_skip) {
      nn_init_zeros_(self$linear1$bias)
      nn_init_zeros_(self$linear2$bias)
      
      nn_init_zeros_(self$linear1$weight)
      nn_init_zeros_(self$linear2$weight)
      
      beta_0 <- torch_tensor(beta_vec[1], dtype = torch_float())
      betas <- torch_tensor(array(beta_vec[2:(input_size_skip + 1)], dim = c(1, input_size_skip)), dtype = torch_float())
      
      self$linear_skip$bias <- nn_parameter(beta_0)
      self$linear_skip$weight <- nn_parameter(betas)
    },
    
    mlp = function(x) {
      x$x_mlp %>% 
        self$bn0() %>%
        self$linear1() %>% 
        nnf_relu() %>%
        self$bn1() %>%
        self$linear2()
    },
    
    skip = function(x) {
      x$x_skip %>% 
        self$bn_skip() %>% 
        self$linear_skip()
    },
    
    forward = function(x) {
      torch_add(self$skip(x), self$mlp(x)) %>% 
        nnf_softplus()
    }
  )
