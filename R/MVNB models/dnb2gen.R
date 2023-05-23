dnb2gen <- function(x, mu, alpha, gamma, ln = F) {
  if (ln == F) {
    c1 <- gamma(x + alpha) / (gamma(x + 1) * gamma(alpha))
    c2 <- (gamma / (gamma + mu)) ^ alpha
    c3 <- (mu / (mu + gamma)) ^ x
    res <- c1 * c2 * c3
  } else if (ln == T) {
    res <- lgamma(x + alpha) - lgamma(x + 1) - lgamma(alpha) + alpha * (log(gamma) - log(gamma + mu)) + x * (log(mu) - log(mu + gamma))
  }
  
  return(res)
}
