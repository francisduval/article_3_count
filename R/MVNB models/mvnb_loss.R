mvnb_loss <- function(mu, alpha, gamma, target) {
  x1 <- -torch_lgamma(target + alpha)
  x2 <- torch_lgamma(alpha)
  x3 <- -alpha * torch_log(gamma)
  x4 <- alpha * torch_log(gamma + mu)
  x5 <- -target * torch_log(mu)
  x6 <- target * torch_log(mu + gamma)
  
  loss_vec <- x1 + x2 + x3 + x4 + x5 + x6
  mean(loss_vec)
}