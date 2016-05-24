################################
# The observed data,
#
#  - The distribution parameters, (Normal distribution means)
#  - The latent variables. (the 'mixture' probabilities)
#
# The EM algorithm bounces back and forth between two processes:
# 
#  1. Given the current parameters and the observed data, estimate the latent variables. 
#  2. Given the observed data and the latent variables, estimate the parameters.
#
# Suppose we flip a biased coin that comes up heads 25% \( (p=0.25) \) of the time. 
# If it's heads, we draw from a Normal distribution with mean 1, standard deviation 1. 
# If it's tails, we draw from a Normal distribution with mean 7, standard deviation 1. 
# We repeat this process 1000 times.

library("ggplot2")
library("lattice")

set.seed(122)
tau_1_true <- 0.30
x <- y <- vector(mode="numeric", length=1000)
for( i in 1:1000 ) {
  if( runif(1) < tau_1_true ) {
    x[i] <- rnorm(1, mean=1)
    y[i] <- "heads"
  } else {
    x[i] <- rnorm(1, mean=3)
    y[i] <- "tails"
  }
}
densityplot( ~x, 
             par.settings = list(
               plot.symbol = list(
                 col=as.factor(y)
               )
             )
)

## set the initial guesses for the distribution parameters
mu_1 <- 0
mu_2 <- 1

## as well as the latent variable parameters
tau_1 <- 0.5
tau_2 <- 0.5

for( i in 1:1000 ) {
  
  ## Given the observed data, as well as the distribution parameters,
  ## what are the latent variables?
  
  T_1 <- tau_1 * dnorm( x, mu_1 )
  T_2 <- tau_2 * dnorm( x, mu_2 )
  
  P_1 <- T_1 / (T_1 + T_2)
  P_2 <- T_2 / (T_1 + T_2) ## note: P_2 = 1 - P_1
  
  tau_1 <- mean(P_1)
  tau_2 <- mean(P_2)
  
  ## Given the observed data, as well as the latent variables,
  ## what are the population parameters?
  
  mu_1 <- sum( P_1 * x ) / sum(P_1)
  mu_2 <- sum( P_2 * x ) / sum(P_2)
  
  ## print the current estimates
  
  print( c(mu_1, mu_2, mean(P_1)) )
  
}