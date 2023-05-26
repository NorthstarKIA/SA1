#Defining of function that calculates probabilities of defects
calculate_defective_probs <- function(x, y) {
  probs <- c((y[1]/x[1]), (y[2]/x[2]), (y[3]/x[3]))
  return(probs)
}

#Define function that validates production rates
validates_x_input <- function(x){
  #Check that x is a numeric vector of length 3
  if (!is.numeric(x) ||  length(x) !=3) {
    stop ("x must be a numeric vector length 3")
  }
  #Check that the sum of x is 1
  if (abs(sum(x)-1) > 1e-10){
    stop("Stop sum of x must be 1")
  }
  #Check that each x_i is between 0.1 and 0.4
  if (any(x<0.1) || any(x > 0.4)) {
    stop("Each x_i must be between 0.1 and 0.4")
  }
}

# Define function that validates defective rates input
validates_y_input <- function(y) {
  #Check that y is a numeric vector of length 3
  if (!is.numeric(y) || length(y) !=3) {
    stop("y must be a numeric vector of length 3")
  }
  #Check that the sum of x is 0.12
  if (abs(sum(x)-0.12) > 1e-10){
    stop("Stop sum of x must be 0.12")
  }
  #Check that each x_i is between 0.1 and 0.5
  if (any(x<0.01) || any(y > 0.5)) {
    stop("Each y_i must be between 0.01 and 0.05")
  }
}

#Get user input for production rates and validate
x <- numeric(3)
for (i in 1:3){
  x[i] <- as.numeric(readline(paste0("Enter production rate for product ",i,": ")))
}
validates_x_input(x)

#get user input for defective rates
y <- numeric(3)
for (i in 1:3){
  y[i] <- as.numeric(readline(paste0("Enter defective rate for product ", i, ": ")))
}
validates_y_input(y)

#Calculate
defective_probs <- calculate_defective_probs(x, y)
cat("Probability of product form Factory 1 defective is", round (defective_probs[1]*100,2), "%n")
cat("Probability of product form Factory 2 defective is", round (defective_probs[2]*100,2), "%n")
cat("Probability of product form Factory 3 defective is", round (defective_probs[3]*100,2), "%n")