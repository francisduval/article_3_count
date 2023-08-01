MVNBCANN3L <-
  nn_module(
    "MVNBCANN3L",
    
    initialize = function(input_size_mlp = 85, input_size_skip = 15, p = 0, n_1L, n_2L, n_3L) {
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
      
      self$linear_phi = nn_linear(1, 1, bias = F)
      
      self$bn_skip = nn_batch_norm1d(input_size_skip)
      self$linear_skip = nn_linear(input_size_skip, 1)
      
      beta_vec = c(
        -2.818449879,
        0.196261410,
        0.020357634,
        0.008058515,
        0.043447884,
        -0.145438109,
        -0.091940396,
        0.177812436,
        -0.029063520,
        0.025198414,
        0.029204766,
        0.093115072,
        0.091970961,
        0.016581202,
        0.003276535,
        -0.017965556
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
      
      # nn_init_constant_(self$linear_phi$weight, val = -0.8548)
      nn_init_constant_(self$linear_phi$weight, val = 3.9261)
      
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
    
    mu = function(x) {
      nnf_softplus(torch_add(self$skip(x), self$mlp(x)))
    },
    
    phi = function() {
      nnf_softplus(self$linear_phi(1))
    },
    
    alpha = function(x) {
      self$phi() + x$x_sum_past_claims
    },
    
    gamma = function(x) {
      self$phi() + x$x_sum_past_mu
    },
    
    forward = function(x) {
      output_mu <- self$mu(x)
      output_phi <- self$phi()
      output_alpha <- self$alpha(x)
      output_gamma <- self$gamma(x)
      
      list(mu = output_mu, phi = output_phi, alpha = output_alpha, gamma = output_gamma)
    }
  )
