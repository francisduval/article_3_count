nb2_loss <- function(mu, phi, target) {
  x1 <- target * torch_log((phi * mu) / (1 + phi * mu))
  x2 <- (1 / phi) * torch_log(1 + phi * mu)
  x3 <- torch_lgamma(target + 1 / phi)
  x4 <- torch_lgamma(1 / phi)
  
  loss_vec <- - (x1 - x2 + x3 - x4)
  mean(loss_vec)
}


