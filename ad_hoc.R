train <- tar_read(train)
valid <- tar_read(valid)

nn_poisson <- tar_read(nn_poisson)
nn_nb2 <- tar_read(nn_nb2)

glm_poisson_class <- tar_read(glm_poisson_class)
glm_poisson_class_tele <- tar_read(glm_poisson_class_tele)

glm_nb2_class <- tar_read(glm_nb2_class)
glm_nb2_class_tele <- tar_read(glm_nb2_class_tele)


nn_poisson$print_metrics()
nn_nb2$print_metrics()
