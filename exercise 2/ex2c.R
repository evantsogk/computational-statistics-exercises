# calculates the value of the Epanechnikov kernel
epanechnikov <- function(x) {
  result <- 0
  if (abs(x) <= 1) {
    result <- 3/4*(1-x**2)
  }
  return (result)
}


# finds optimal h maximizing cross-validated likelihood
find_hopt <- function(x, h_values, kernel) {
  n <- length(x)
  ML <- -Inf
  hopt <- h_values[1]
  for (h in h_values) {
    # calculate cross validated likelihood for h
    L <- 0
    for (i in 1:n) {
      # calculate kernel values without xi
      k <- sapply((x[-i]-x[i])/h, kernel)
      L <- L + log(sum(k))
    }
    L <- 1/n*L - log((n-1)*h)
    
    # check if likelihood is greater
    if (L > ML) {
      ML <- L
      hopt <- h
    }
  }
  return (hopt)
}


# KDE given a sample x
KDE <- function(x, h, kernel) {
  n <- length(x)
  f_est <- rep(NA, n)
  for (i in 1:n) {
    k <- sapply((x-x[i])/h, kernel)
    f_est[i] <- 1/(n*h)*sum(k)
  }
  return (f_est)
}


# f(x)
f <- function(x) {
  return (exp(x)/(exp(3)-1))
}



# inverse transform sampling
u <- runif(100, 0, 1)
x <- log((exp(3)-1)*u + 1)


# find optimal h
hopt <- find_hopt(x, seq(0.01, 0.5, 0.01), epanechnikov)
hopt


# plot estimation of f(x) along with f(x)
plot(sort(x), KDE(sort(x), hopt, epanechnikov), type="l", col="orange", lwd=2, 
     main="Kernel Density Estimation", xlab="x", ylab="Density", xlim=c(0, 3), 
     ylim=c(0, 1))
curve(f, add=TRUE, col="blue", lwd=2)
legend(0,1,c("f(x)", "KDE"), lwd=c(2,2), col=c("blue", "orange"))






