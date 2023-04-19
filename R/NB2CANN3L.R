NB2CANN3L <- 
  nn_module(
    "NB2CANN3L",
    
    initialize = function(input_size_mlp = 86, input_size_skip = 16, p = 0, n_1L, n_2L, n_3L) {
      self$bn0 = nn_batch_norm1d(input_size_mlp)
      self$linear1 = nn_linear(input_size_mlp, n_1L)
      self$bn1 = nn_batch_norm1d(n_1L)
      self$linear2 = nn_linear(n_1L, n_2L)
      self$bn2 = nn_batch_norm1d(n_2L)
      self$linear3 = nn_linear(n_2L, n_3L)
      self$bn3 = nn_batch_norm1d(n_3L)
      self$linear4 = nn_linear(n_3L, 1)
      
      self$do1 = nn_dropout(p = p)
      self$do2 = nn_dropout(p = p)
      self$do3 = nn_dropout(p = p)
      
      self$disp = nn_parameter(torch_tensor(0.5, dtype = torch_float()))
      
      self$bn_skip = nn_batch_norm1d(input_size_skip)
      self$linear_skip = nn_linear(input_size_skip, 1)
      
      beta_vec = c(
        -3.315941880194, 
        0.616156785235, 0.000002915498, 0.000824747918, 0.113638267928,
        -0.036712764040, -0.003624109271, -0.001962461408, 0.000020006844, 
        -0.057640242691, 0.053808785455, 0.128105697562, 0.245872331710, 
        0.183671264351, 0.132037363102, 0.007204221409, -0.089563765999
      )
      self$init_params(beta_vec, input_size_skip)
    },
    
    init_params = function(beta_vec, input_size_skip) {
      torch_manual_seed(1994)
      
      nn_init_normal_(self$linear1$bias, std = 0.01)
      nn_init_normal_(self$linear2$bias, std = 0.01)
      nn_init_normal_(self$linear3$bias, std = 0.01)
      nn_init_normal_(self$linear4$bias, std = 0.01)
      
      nn_init_normal_(self$linear1$weight, std = 0.01)
      nn_init_normal_(self$linear2$weight, std = 0.01)
      nn_init_normal_(self$linear3$weight, std = 0.01)
      nn_init_normal_(self$linear4$weight, std = 0.01)
      
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
        self$do1() %>% 
        self$bn1() %>%
        self$linear2() %>% 
        nnf_relu() %>%
        self$do2() %>% 
        self$bn2() %>%
        self$linear3() %>% 
        nnf_relu() %>%
        self$do3() %>% 
        self$bn3() %>%
        self$linear4()
    },
    
    skip = function(x) {
      x$x_skip %>% 
        self$bn_skip() %>% 
        self$linear_skip()
    },
    
    forward = function(x) {
      nb_obs <- dim(x$x_mlp)[1]
      
      output_mu <- nnf_softplus(torch_add(self$skip(x), self$mlp(x)))
      output_disp <- nnf_softplus(torch_full(c(nb_obs, 1), self$disp))
      
      torch_cat(list(output_mu, output_disp), dim = 2)
    }
  )
