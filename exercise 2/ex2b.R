N <- 1000
x <- rep(NA, N)  # final simulated values

# rejection sampling
for(i in 1:N) {
  while(TRUE) {
    # y ~ U(0, 3)
    y <- runif(1, 0, 3)
  
    # u ~ U(0, 1)
    u <- runif(1, 0, 1)
  
    # accept value
    if (u <= 3*exp(y) / (3.16 * (exp(3)-1))) {
      x[i] <- y
      break
    }
  }
}

# plot histogram
hist(x, probability = TRUE)

# plot PDF
f <- function(x) {(exp(x))/(exp(3)-1)}
curve(f, add=TRUE)


