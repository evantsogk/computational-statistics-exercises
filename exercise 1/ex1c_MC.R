for(N in c(100, 1000)) {
  for(a in 0:4) {
    
    # simulate N values from normal distribution with mean = a and sd=1
    x <- rnorm(N, a, 1)
    
    # Monte Carlo estimator of J
    J_est <- 1/length(x) * sum((x+a)**2 * exp(1/2*(a**2 - 2*a*x)))
    
    # actual value of J
    J <- 1 + a**2
    
    
    cat("\n\nN =", N, "a =", a)
    cat("\nEstimated J =", J_est)
    cat("\nTrue J =", J)
  }
}


