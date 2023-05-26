#function to calculate mean and variance of a univariate discrete random variable
mean_variance <- function(x, prob) {
# validate probabilities
  if (!all(prob >= 0 & prob <= 1)) {
    stop("Probabilities must be in the interval [0, 1]")
  }
# validate sum of probabilities
  if (abs(sum(prob) - 1) > 1e-8) {
    stop("Probabilities must sum to one")
  }
# calculate mean and variance
  mean <- sum(x * prob)
  variance <- sum((x - mean)^2 * prob)
# return mean and variance as a list
  list(mean = mean, variance = variance)
}

# function to plot the PDF and CDF of a univariate discrete random variable
plot_pdf_cdf <- function(x, prob) {
# validate probabilities
  if (!all(prob >= 0 & prob <= 1)) {
    stop("Probabilities must be in the interval [0, 1]")
  }
# validate sum of probabilities
  if (abs(sum(prob) - 1) > 1e-8) {
    stop("Probabilities must sum to one")
  }
# calculate PDF and CDF
  pdf <- prob
  cdf <- cumsum(prob)
# plot PDF and CDF
  plot(x, pdf, type = "h", lwd = 2, xlab = "x", ylab = "Probability", main = "PDF and CDF")
  lines(x, cdf, type = "s", lwd = 2)
  legend("topright", c("PDF", "CDF"), lwd = 2, col = c("black", "red"), bty = "n")
}

# function to calculate the marginal and conditional distributions of a bivariate discrete random variable
marginal_conditional <- function(x, y, prob) {
# check that the probabilities sum up to one
  if (sum(prob) != 1) {
    stop("Probabilities do not sum up to one.")
  }
# check that the probabilities are between 0 and 1
  if (any(prob < 0) | any(prob > 1)) {
    stop("Probabilities must be between 0 and 1.")
  }
# calculate the marginal distributions
  px <- apply(prob, 1, sum)
  py <- apply(prob, 2, sum)
# calculate the conditional distributions
  py_given_x <- t(t(prob) / px)
  px_given_y <- prob / py
# create a list containing the marginal and conditional distributions
  list(px = px, py = py, py_given_x = py_given_x, px_given_y = px_given_y)
}


# function to plot the PDF and CDF of a bivariate discrete random variable
# function to plot the pdf and cdf of a bivariate discrete random variable
plot_bivariate_pdf_cdf <- function(x, y, prob) {
# check that the probabilities sum up to one
  if (sum(prob) != 1) {
    stop("Probabilities do not sum up to one.")
  }
# check that the probabilities are between 0 and 1
  if (any(prob < 0) | any(prob > 1)) {
    stop("Probabilities must be between 0 and 1.")
  }
# calculate the marginal distributions
  px <- apply(prob, 1, sum)
  py <- apply(prob, 2, sum)
# calculate the pdf and cdf
  pdf <- matrix(prob, nrow = length(x), ncol = length(y), byrow = TRUE)
  cdf <- apply(pdf, c(1, 2), cumsum)
# plot the pdf and cdf
  persp(x, y, pdf, theta = -30, phi = 30, xlab = "X", ylab = "Y", zlab = "PDF", main = "Bivariate PDF")
  persp(x, y, cdf, theta = -30, phi = 30, xlab = "X", ylab = "Y", zlab = "CDF", main = "Bivariate CDF")
# plot the marginal pdf and cdf for x
  plot(x, px, type = "h", xlab = "X", ylab = "PDF", main = "Marginal PDF for X")
  plot(x, cumsum(px), type = "h", xlab = "X", ylab = "CDF", main = "Marginal CDF for X")
# plot the marginal pdf and cdf for y
  plot(y, py, type = "h", xlab = "Y", ylab = "PDF", main = "Marginal PDF for Y")
  plot(y, cumsum(py), type = "h", xlab = "Y", ylab = "CDF", main = "Marginal CDF for Y")
}


# function to calculate the mean and variance of a bivariate discrete random variable
mean_variance_bivariate <- function(x, y, prob) {
# check that the probabilities sum up to one
  if (sum(prob) != 1) {
    stop("Probabilities do not sum up to one.")
  }
# check that the probabilities are between 0 and 1
  if (any(prob < 0) | any(prob > 1)) {
    stop("Probabilities must be between 0 and 1.")
  }
# calculate the marginal distributions
  px <- apply(prob, 1, sum)
  py <- apply(prob, 2, sum)
# calculate the means
  mean_x <- sum(x * px)
  mean_y <- sum(y * py)
# calculate the variances
  var_x <- sum((x - mean_x) ^ 2 * px)
  var_y <- sum((y - mean_y) ^ 2 * py)
# calculate the covariance
  cov_xy <- sum((outer(x, y, "*") - mean_x * mean_y) * prob)
# return the means, variances, and covariance as a list
  return(list(mean_x = mean_x, mean_y = mean_y, var_x = var_x, var_y = var_y, cov_xy = cov_xy))
}


# example of using the functions for a univariate discrete random variable
x <- c(1, 1.1, 1.5)
prob <- c(0.2, 0.4, 0.4)
mean_variance(x, prob)
plot_pdf_cdf(x, prob)

# example of using the functions for a bivariate discrete random variable
x <- c(1, 2, 3)
y <- c(4, 5, 6)
prob <- matrix(c(0.1, 0.1, 0.2, 0.2, 0.1, 0.1, 0.1, 0.1, 0.0), nrow = 3, ncol = 3, byrow = T)
mean_variance_bivariate(x, y, prob)
plot_bivariate_pdf_cdf(x, y, prob)
marginal_conditional(x, y, prob)