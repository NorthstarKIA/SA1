# Define probability of finding the key phrase on each site
p <- 0.6
p
# Function to simulate a search until the key phrase is found
search_until_found <- function(p) {
  n_searches <- 0
  found <- FALSE
  while (!found) {
    n_searches <- n_searches + 1
    found <- runif(1) < p
  }
  n_searches
}

#10,000 searches
set.seed(123)
n_sim <- 10000
sims <- replicate(n_sim, search_until_found(p))

#Calculate mean and variance
mean_sim <- mean(sims)
variance_sim <- var(sims)

#Plot pdf
hist(sims, prob = TRUE, breaks = seq(0.5, max(sims)+0.5, by = 1),
     main = "Simulated PDF of Searches until Key Phrase is Found",
     xlab = "Number of Searches", ylab = "Probability")
lines(density(sims), lwd = 2)

#Calculate the conditional distribution if three unsuccessful searches
subset_sims <- sims[sims > 3]
conditional_sims <- subset_sims - 3

#Calculate mean and variance (conditional distribution)
mean_con <- mean(conditional_sims)
variance_con <- var(conditional_sims)

# Plot the simulated conditional pdf
hist(conditional_sims, prob = TRUE, breaks = seq(0.5, max(conditional_sims)+0.5, by = 1),
     main = "Simulated Conditional PDF of Searches until Key Phrase is Found (Given X > 3)",
     xlab = "Number of Searches", ylab = "Probability")
lines(density(conditional_sims), lwd = 2)

# Check Markov memory-less property
p_x4_given_xgt3 <- sum(sims == 4 & sims > 3) / sum(sims > 3)
p_x1 <- sum(sims == 1) / n_sim
p_x5_given_xgt3 <- sum(sims == 5 & sims > 3) / sum(sims > 3)
p_x2 <- sum(sims == 2) / n_sim

# Printing of results
cat("Mean of simulated distribution:", mean_sim, "\n")
cat("Variance of simulated distribution:", variance_sim, "\n")
cat("Mean of simulated conditional distribution:", mean_con, "\n")
cat("Variance of simulated conditional distribution:", variance_con, "\n")
cat("P(X=4|X>3):", p_x4_given_xgt3, "\n")
cat("P(X=1):", p_x1, "\n")
cat("P(X=5|X>3):", p_x5_given_xgt3, "\n")
cat("P(X=2):", p_x2, "\n")