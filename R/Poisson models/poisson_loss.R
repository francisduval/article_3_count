poisson_loss <- function(mu, target) {
  loss_vec <- mu - target * torch_log(mu) + torch_lgamma(target + 1)
  mean(loss_vec)
}