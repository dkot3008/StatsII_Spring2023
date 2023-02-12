#Heres an R function that implements the Kolmogorov-Smirnov test for the normal distribution:

kolmogorov.smirnov.test <- function(data) {
  # create empirical distribution of observed data
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  # generate test statistic
  D <- max(abs(empiricalCDF - pnorm(data)))
  # calculate p-value
  p.value <- 2 * (1 - pnorm(D * sqrt(length(data))))
  # return test statistic and p-value
  return(c(D = D, p.value = p.value))
}

set.seed(123)
dat <- rcauchy(1000, location = 0, scale = 1)

kolmogorov.smirnov.test(dat)

This will return the test statistic and the p-value. If the p-value is below a certain threshold (e.g. 0.05), then the hypothesis that the empirical distribution of the data matches the normal distribution can be rejected.


#Question 2

Heres an example of how to estimate an OLS regression in R using the Newton-Raphson algorithm with BFGS, and how to compare the results to those obtained using the lm function:
  
  # Set the seed
  set.seed(55)

# Generate data
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75 * data$x + rnorm(200, 0, 1.5)

# Estimate OLS regression using the lm function
lm_fit <- lm(y ~ x, data = data)

# Estimate OLS regression using the BFGS algorithm
library(numDeriv)

# Define the negative log-likelihood function
neg_log_lik <- function(b, X, y) {
  mu <- X %*% b
  -sum(dnorm(y, mean = mu, sd = 1.5, log = TRUE))
}

# Define the gradient of the negative log-likelihood function
grad_neg_log_lik <- function(b, X, y) {
  mu <- X %*% b
  t(X) %*% (mu - y) / 1.5^2
}

# Define the starting values for the parameters
b0 <- c(0, 0)

# Fit the model using BFGS
bfgs_fit <- optim(b0, neg_log_lik, X = cbind(1, data$x), y = data$y,
                  method = "BFGS", gr = grad_neg_log_lik)

# Compare results
cbind(lm_fit$coefficients, bfgs_fit$par)

The results obtained using the lm function and the BFGS algorithm should be equivalent.
In this case, both methods estimate the intercept and slope coefficients to be very close to 0 and 2.75, respectively.
