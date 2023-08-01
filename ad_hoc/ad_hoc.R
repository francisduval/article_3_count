glm_nb2_class_learn <- tar_read(glm_nb2_class_learn)

# ===============================================================================================================================

glm_nb2_class_learn$train_res$phi

glm_mvnb_class_tele_learn$betas %>% round(4)

glm_nb2_class_tele_learn$params_df %>%
  slice(1:8, 22:29) %>% 
  arrange(term) %>% pull(estimate) %>% round(4) %>% as.numeric()


# Créer des données simulées
set.seed(123)
mu <- 5
theta <- 2 # mu = 5 et theta = 2, donc on s'entend que Var(Y) = 5 + 5^2/2 = 17.5
data <- data.frame(y = rnbinom(100000, mu = mu, size = theta))

# Ajuster le modèle
model <- glm.nb(y ~ 1, data)

# Obtenir les moyennes ajustées du modèle
fitted_means <- predict(model, type = "response")

# Calculer les variances prédites en utilisant l'équation
predicted_variances <- fitted_means + fitted_means ^ 2 / theta
predicted_variances

# ===============================================================================================================================

glm_nb2_class_learn <- tar_read(glm_nb2_class_learn)
glm_nb2_class_learn$print_metrics()
glm_nb2_class_learn$train_res$phi
glm_nb2_class_learn$params_df %>% 
  # slice(1:8, 22:29) %>% 
  arrange(term) %>% pull(estimate) %>% round(4) %>% as.numeric()

glm_nb2_class_tele_learn <- tar_read(glm_nb2_class_tele_learn)
glm_nb2_class_tele_learn$print_metrics()
glm_nb2_class_tele_learn$train_res$phi

# ===============================================================================================================================

glm_nb2_class_tele_learn <- tar_read(glm_nb2_class_tele_learn)
glm_nb2_class_tele_learn$print_metrics()
