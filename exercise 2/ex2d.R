# the test function T
test_function <- function(x) {
  return (abs(mean(x) - m_h0))
}

# returns the estimator values of every bootstrap sample
bootstrap <- function(x, B, estimator) {
  thetas <- rep(NA, B)
  for (b in 1:B) {
    bootstrap <- sample(x, replace = TRUE)
    thetas[b] <- estimator(bootstrap)
  }
  return (thetas)
}


# inverse transform sampling
n <- 10
u <- runif(10, 0, 1)
x <- log((exp(3)-1)*u + 1)

m_h0 <- 2 # null hypothesis
m <- mean(x)  # sample mean
t <- test_function(x) # test function T for the sample
cat("mean =", m)
cat("T =", t)

# center sample according to h0
if (m < m_h0) {
  x_new <- x + t
} else {
  x_new <- x - t
}

# bootstrap estimates of T
B <- 1000
t_boot <- bootstrap(x_new, B, test_function)

# calculate p-value
pvalue <- (sum(t_boot[t_boot > t])+1) / (B+1)
cat("p-value =", pvalue)

# find 95% confidence interval
a <- 0.05
m_boot <- bootstrap(x, B, mean)
m_boot <- sort(m_boot)
quantile1 <- m_boot[as.integer(round(a/2*B))]
quantile2 <- m_boot[as.integer(round((1-a/2)*B))]
cat("CI: (", quantile1, ', ', quantile2, ")", sep='')

# histogram of means
hist(m_boot, prob=T)






