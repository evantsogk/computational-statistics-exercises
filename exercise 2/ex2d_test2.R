# the test function T
test_function <- function(m, m_h0) {
  return (abs(m - m_h0))
}

# returns the T value for every bootstrap sample
bootstrap_t <- function(x, B, m_h0) {
  T_est <- rep(NA, B) # initialize T estimates
  # perform bootstrap sampling
  for (b in 1:B) {
    # sample with replacement
    bootstrap <- sample(x, replace = TRUE)
    # estimate T from the bootstrap sample
    T_est[b] <- test_function(mean(bootstrap), m_h0)
  }
  return (T_est)
}

# returns the mean values of every bootstrap sample
bootstrap_m <- function(x, B) {
  m_est <- rep(NA, B) # initialize T estimates
  # perform bootstrap sampling
  for (b in 1:B) {
    # sample with replacement
    bootstrap <- sample(x, replace = TRUE)
    # estimate T from the bootstrap sample
    m_est[b] <- mean(bootstrap)
  }
  return (m_est)
}

# inverse transform sampling
n <- 10
u <- runif(10, 0, 1)
x <- log((exp(3)-1)*u + 1)

m_h0 <- 2  # null hypothesis mean
m <- mean(x)  # sample mean
cat("Sample mean:", m)

t <- test_function(m, m_h0) # test function T for the sample
cat("T =", t)

# center sample according to h0
if (m < m_h0) {
  x <- x + t
} else {
  x <- x - t
}

# bootstrap estimates of T
B <- 1000
t_boot <- bootstrap_t(x, B, m_h0)

# calculate p-value
pvalue <- (sum(t_boot[t_boot > t])+1) / (B+1)
cat("p-value =", pvalue)

# check if m_ho is in 95% confidence interval
a <- 0.05
m_boot <- bootstrap_m(x,)
m_boot <- sort(m_boot)
quantile1 <- m_boot[as.integer(round(a/2*B))]
quantile2 <- m_boot[as.integer(round((1-a/2)*B))]
cat("(", quantile1, ', ', quantile2, ")", sep='')

