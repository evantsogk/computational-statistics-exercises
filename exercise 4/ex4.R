library(data.table)
library(glmnet)

n=50
vnames <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11', 
            'X12', 'X13', 'X14', 'X15')

# simulate values for the variables X1-X10
data <- data.table(NULL)
for (v in vnames[1:10]) {
  data[, v] <- rnorm(n)
}

# simulate values for the variables X11-X15
for (v in vnames[11:15]) {
  for (i in 1:n) {
    mean <- 0.2*data[[i, 'X1']] + 0.4*data[[i, 'X2']] + 0.6*data[[i, 'X3']] + 
      0.8*data[[i, 'X4']] + 1.1*data[[i, 'X5']]
    data[i, v] <- rnorm(1, mean, 1)
  }
}

# simulate values for the response variable Y
for (i in 1:n) {
  mean <- 4 + 2*data[[i, 'X1']] - data[[i, 'X5']] + 2.5*data[[i, 'X7']] + 
    1.5*data[[i, 'X11']] + 0.5*data[[i, 'X13']]
  data[i, 'Y'] <- rnorm(1, mean, 1.5)
}

# function to perform full enumeration of the model space using BIC
bestLM <- function(xnames, yname, data) {
  # fit null model
  best_model <- lm(paste(yname, "~", 1), data = data)
  min_bic <- BIC(best_model)
  
  for (m in 1:length(xnames)) {
    combinations <- combn(xnames, m)  # get combinations m at a time
    for (c in 1:ncol(combinations)) {
      # fit a model for each combination
      model <- lm(paste(paste(yname, "~"), 
                        paste(combinations[, c], collapse="+")), data = data)
      bic <- BIC(model)
      
      # check if this model is better
      if (bic < min_bic) {
        min_bic <- bic
        best_model <- model
      }
    }
  }
  
  return (list(best_model, min_bic))
}


# find best model
results <- bestLM(vnames, 'Y', data)
results


############################################################################


# perform Lasso
lasso <- glmnet(data[, ..vnames], data$Y)
plot(lasso, label=T, xvar='lambda')


# perform cross-validation to find best lambda
lasso_cv <- cv.glmnet(as.matrix(data[, ..vnames]), data$Y, nfolds = 10, type.measure='mse')
plot(lasso_cv)
lasso_cv


# get coefficients
blasso <- coef(lasso_cv, s="lambda.min")
blasso

# full model
mfull <- lm(paste('Y ~', paste(vnames, collapse="+")), data = data)

# standardize
zblasso <-blasso[-1] * apply(as.matrix(data[, ..vnames]), 2, sd)
zbols <- coef(mfull)[-1] * apply(as.matrix(data[, ..vnames]), 2, sd)

# calculate shrinkage factor
s <- sum(abs(zblasso)) / sum(abs(zbols))
s







