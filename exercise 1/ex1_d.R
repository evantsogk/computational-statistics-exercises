# calculates the standard error of J Monte Carlo estimator using the bootstrap method
bootstrap_se <- function(x, B, a) {
  
  # initialize J estimates
  J_est <- rep(NA, B)
  # perform bootstrap sampling
  for (b in 1:B) {
    # sample with replacement
    bootstrap <- sample(x, replace = TRUE)
    # estimate J from the bootstrap sample
    J_est[b] <- (1/length(bootstrap) * sum((bootstrap+a)**2))
  }
  
  # calculate standard error
  se <- sqrt(1/(B-1) * sum((J_est - mean(J_est))**2))
  return (se)
}


N <- 1000
a <- 4
B <- 1000 # number of bootstrap samples
# simulate N values from the standard normal distribution
x <- rnorm(N, 0, 1)

# estimated standard error using the bootstrap method
se_est <- bootstrap_se(x, B, a)

# estimate theoretical standard error
se_th <- sqrt((4*a**2 + 2) / N)

cat("Bootstrap se =", se_est)
cat("Theoretical se =", se_th)




