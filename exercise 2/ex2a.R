# simulate 1000 values from the uniform distribution
u <- runif(1000, 0, 1)

# inverse transform sampling
x <- log((exp(3)-1)*u + 1)

# plot histogram
hist(x, probability = TRUE)

# plot pdf
f <- function(x) {(exp(x))/(exp(3)-1)}
curve(f, add=TRUE)

