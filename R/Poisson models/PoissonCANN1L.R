PoissonCANN1L <- 
  nn_module(
    "PoissonCANN1L",
    
    initialize = function(input_size_mlp = 86, input_size_skip = 16, n_1L) {
      self$bn0 = nn_batch_norm1d(input_size_mlp)
      self$linear1 = nn_linear(input_size_mlp, n_1L)
      self$bn1 = nn_batch_norm1d(n_1L)
      self$linear2 = nn_linear(n_1L, 1)
      
      self$bn_skip = nn_batch_norm1d(input_size_skip)
      self$linear_skip = nn_linear(input_size_skip, 1)
      
      beta_vec = c(
        -2.821575625,
        0.194967088,
        0.019056239,
        0.007703927,
        0.042412745,
        -0.146902033,
        -0.064365497,
        -0.033364018,
        0.179616550,
        -0.028814077,
        0.024572095,
        0.028071549,
        0.091785046,
        0.091836265,
        0.016965687,
        0.003538139,
        -0.018009292
      )
      
      self$init_params(beta_vec, input_size_skip)
    },
    
    init_params = function(beta_vec, input_size_skip) {
      torch_manual_seed(1994)
      
      nn_init_normal_(self$linear1$bias, std = 0.01)
      nn_init_normal_(self$linear2$bias, std = 0.01)
      
      nn_init_normal_(self$linear1$weight, std = 0.01)
      nn_init_normal_(self$linear2$weight, std = 0.01)
      
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
