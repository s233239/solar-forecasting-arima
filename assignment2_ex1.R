# load excel read library
library(utils)
library(ggplot2)
library(xtable)

# clear variables
rm(list=ls())

# resizing the graphics window
par(mfrow=c(1,1))


## ---------------------------------------------------------------------------------------
## Part 1 - Stability
## 1.4. Plot the autocorrelation ρ(k) up to nlag = 30 for the coefficient values above.

# AR(2) process coefficients
phi1 = -0.7
phi2 = -0.2

nlag = 30

# Store the values of the autocorrelation ρ(k)
acf_data <- data.frame(
  time = 0:nlag, 
  rho = rep(0, nlag+1)
)

# Base cases of the recursive formula
acf_data$rho[1] <- 1
acf_data$rho[2] <- -phi1 / (1+phi2)

# Calculate recursively ρ(k)
for (i in 3:(nlag+1)){
  acf_data$rho[i] <- - phi1 * acf_data$rho[i-1] - phi2 * acf_data$rho[i-2]
}

# Create LaTeX table code using xtable
latex_code <- xtable(acf_data, digits = 2)

# Print the LaTeX code
print(latex_code, include.rownames = FALSE, only.contents = TRUE)

# Plot the autocorrelation
plot(acf_data$time, acf_data$rho, type = "h", lwd = 2, xlab = "Time", ylab = "Value", main = "Autocorrelation Plot for ϕ1 = −0.7 and ϕ2 = −0.2")
#barplot(acf_data$rho, names.arg = acf_data$time, width=1, xlab = "Time", ylab = "Value", main = "Autocorrelation Plot for ϕ1 = −0.7 and ϕ2 = −0.2")


## ---------------------------------------------------------------------------------------
## 1.5. Simulate 5 realizations of the process up to n = 200 observations. Plot them in a plot.

# Function for plot
plotit <- function(x){
  layout(rbind(1,2:3))
  par(mar=c(3,3,1,1), mgp=c(2, 0.7,0))
  plot(x, ylab="X")
  acf(x, lag.max=50, lwd=2)
  pacf(x, lag.max=50, lwd=2)
}

# AR(2) process
model <- list(ar=c(-phi1, -phi2))

n <- 200 # number of observations

# Create a dataframe to store the realizations
ar_data <- data.frame(
  time = 1:n
)

# # Create a dataframe to store the confidence intervals
# ci_data <- data.frame(
#   time = 1:n
# )

# Simulate 5 realizations and plot each
for (i in 1:5) {
  col_name <- paste0("Realization ", i)  # column name
  ar_data[col_name] <- rep(0, n)  # to keep the data of our AR simulated processes
  
  (seed_name <- paste0(i,i+1,i+2))    # set seed for simulation of process
  set.seed(seed_name)
  ar_data[col_name] <- arima.sim(model, n) # simulate an AR process
  
  #ci_data[col_name] <- confint(ar_data[col_name])
  
  plotit( ar_data[col_name] )     # plot the realization
}

# Resizing the graphics window
par(mfrow=c(1,1))

# Plot in one plot
matplot(ar_data$time, ar_data[, -1], type = "l", lty = 1, col = 1:5, xlab = "Time", ylab = "Value",
        main = "Simulated processes for ϕ1 = −0.7 and ϕ2 = −0.2", ylim = range(ar_data[, -1]))
legend("topright", legend = colnames(ar_data[, -1]), col = 1:5, lty = 1, cex = 0.8)


## ---------------------------------------------------------------------------------------
## 1.6. Calculate the empirical ACF of the simulations and plot them together with ρ(k), up to lag 30.


# Calculate the empirical autocorrelation for each realizations
for (i in 1:5) {
  col_name <- paste0("Realization ", i)
  acf_data[col_name] <- acf(ar_data[col_name], lag.max = nlag, plot = FALSE)$acf
}


# # Plot autocorrelation for each model on a single bar plot
# acf_mat <- as.matrix(acf_data[, -1])  # Excluding the first column
# 
# # Generating different colors for each plot
# colors <- c("green", "blue", "purple", "red", "orange", "black") 
# 
# barplot(t(acf_mat), names.arg = acf_data$time, beside = TRUE, col = colors, border=NA, space=c(0.5,5), 
#         xlab = "Time", ylab = "Value", main = "Autocorrelation Plot for ϕ1 = −0.7 and ϕ2 = −0.2")
# legend("topright", legend = colnames(acf_mat), col = colors, lwd = 2)


# Plot on a line plot
matplot(acf_data$time, acf_data[,-1], type = c("h", rep("l", 5)), lty = c(1,rep(1,5)), lwd = c(2,rep(1,5)), col = 1:5, xlab = "Time", ylab = "Value",
        main = "Autocorrelation Plot for ϕ1 = −0.7 and ϕ2 = −0.2", ylim = range(acf_data[, -1]))
legend("topright", legend = c("Theoretical ACF", paste(colnames(acf_data[,-1][,-1]),"ACF")), col = 1:5, lty = c(1,rep(1,5)), lwd = c(2,rep(1,5)), cex = 0.8)

# # Add the confidence interval
# acf_data$CI <- rep(qnorm(0.975), nlag+1)
# lines(acf_data$time, acf_data$CI, type = "l", lwd = 2, lty = 2,col="red")



# y_min <- min(acf_data[,-1])
# y_max <- max(acf_data[,-1])
# 
# plot(acf_data$time, acf_data$rho, type = "l", col = colors[1], lty = 1, ylim = c(y_min, y_max),
#      xlab = "Time", ylab = "Value", main = "Autocorrelation Plot")
# 
# for (i in 1:5) {
#   lines(acf_data$time, acf_mat[,i+1], type = "l", col = colors[i+1])
# }
# legend("topright", legend = colnames(acf_mat), col = colors, lwd = 2)




## ---------------------------------------------------------------------------------------
## 1.7. Redo plots of simulations and ACFs for the following ϕ1-values. 
## ϕ1 = −0.2

phi1 <- -0.2
ar_data2 <- data.frame( time = 1:n )
acf_data2 <- data.frame( time = 0:nlag, 
                         rho = rep(0, nlag+1) )

# Base cases of the recursive formula
acf_data2$rho[1] <- 1
acf_data2$rho[2] <- -phi1 / (1+phi2)

# Calculate recursively ρ(k)
for (i in 3:(nlag+1)){
  acf_data2$rho[i] <- -phi1*acf_data2$rho[i-1] - phi2*acf_data2$rho[i-2] 
}

# AR(2) process
model <- list(ar=c(-phi1, -phi2))

# Simulate 5 realizations
for (i in 1:5) {
  col_name <- paste0("Realization ", i)  
  ar_data2[col_name] <- rep(0, n)  
  
  (seed_name <- paste0(i,i+1,i+2))    # set seed for simulation of process
  set.seed(seed_name)
  ar_data2[col_name] <- arima.sim(model, n) # simulate an AR process
  plotit(ar_data2[col_name])
}

# Resizing the graphics window
par(mfrow=c(1,1))

# Plot the realizations
matplot(ar_data2$time, ar_data2[, -1], type = "l", lty = 1, col = 1:5, xlab = "Time", ylab = "Value",
        main = "Simulated processes for ϕ1 = −0.2", ylim = range(ar_data2[, -1]))
legend("topright", legend = colnames(ar_data[, -1]), col = 1:5, lty = 1, cex = 0.8)


# Calculate the empirical autocorrelation for each realization
for (i in 1:5) {
  col_name <- paste0("Realization ", i)
  acf_data2[col_name] <- acf(ar_data2[col_name], lag.max = nlag, plot = FALSE)$acf
}

# Plot the auto correlations on a line plot
matplot(acf_data2$time, acf_data2[,-1], type = c("h", rep("l", 5)), lty = c(1,rep(1,5)), lwd = c(2,rep(1,5)), col = 1:5, xlab = "Time", ylab = "Value",
        main = "Autocorrelation Plot for ϕ1 = −0.2", ylim = range(acf_data2[, -1]))
legend("topright", legend = c("Theoretical ACF", paste(colnames(acf_data[,-1][,-1]),"ACF")), col = 1:5, lty = c(1,rep(1,5)), lwd = c(2,rep(1,5)), cex = 0.8)


# # On a  bar plot
# acf_mat2 <- as.matrix(acf_data2[, -1]) 
# barplot(t(acf_mat2), names.arg = acf_data2$time, beside = TRUE, col = colors, border=NA, space=c(0.5,5), 
#         xlab = "Time", ylab = "Value", main = "Autocorrelation Plot for ϕ1 = −0.2")
# legend("topright", legend = colnames(acf_mat2), col = colors, lwd = 2)



## ---------------------------------------------------------------------------------------
## 1.8. Redo plots of simulations and ACFs for the following ϕ1-values. 
## ϕ1 = 0.7

phi1 <- 0.7
ar_data2 <- data.frame( time = 1:n )
acf_data2 <- data.frame( time = 0:nlag, 
                         rho = rep(0, nlag+1) )

# Base cases of the recursive formula
acf_data2$rho[1] <- 1
acf_data2$rho[2] <- -phi1 / (1+phi2)

# Calculate recursively ρ(k)
for (i in 3:(nlag+1)){
  acf_data2$rho[i] <- -phi1*acf_data2$rho[i-1] - phi2*acf_data2$rho[i-2] 
}

# AR(2) process
model <- list(ar=c(-phi1, -phi2))

# Simulate 5 realizations
for (i in 1:5) {
  col_name <- paste0("Realization ", i)  
  ar_data2[col_name] <- rep(0, n)  
  
  (seed_name <- paste0(i,i+1,i+2))    # set seed for simulation of process
  set.seed(seed_name)
  ar_data2[col_name] <- arima.sim(model, n) # simulate an AR process
  plotit(ar_data2[col_name])
}

# Resizing the graphics window
par(mfrow=c(1,1))

# Plot the realizations
matplot(ar_data2$time, ar_data2[, -1], type = "l", lty = 1, col = 1:5, xlab = "Time", ylab = "Value",
        main = "Simulated processes for ϕ1 = 0.7", ylim = range(ar_data2[, -1]))
legend("topright", legend = colnames(ar_data[, -1]), col = 1:5, lty = 1, cex = 0.8)


# Calculate the empirical autocorrelation for each realization
for (i in 1:5) {
  col_name <- paste0("Realization ", i)
  acf_data2[col_name] <- acf(ar_data2[col_name], lag.max = nlag, plot = FALSE)$acf
}

# Plot the auto correlations on a line plot
matplot(acf_data2$time, acf_data2[,-1], type = c("h", rep("l", 5)), lty = c(1,rep(1,5)), lwd = c(2,rep(1,5)), col = 1:5, xlab = "Time", ylab = "Value",
        main = "Autocorrelation Plot for ϕ1 = 0.7", ylim = range(acf_data2[, -1]))
legend("topright", legend = c("Theoretical ACF", paste(colnames(acf_data[,-1][,-1]),"ACF")), col = 1:5, lty = c(1,rep(1,5)), lwd = c(2,rep(1,5)), cex = 0.8)




## ---------------------------------------------------------------------------------------
## NON-STATIONARY PROCESSES
## ---------------------------------------------------------------------------------------

# A simulation function for ARMA simulation, use model as arima.sim, i.e. flip sign of phi (into ar) coefficients
sim <- function(model, n, nburnin=100){
  n <- n + nburnin
  # Take the ar and ma part
  ar <- model$ar
  ma <- model$ma
  # The order (i.e. the number of lags)
  p <- length(ar)
  q <- length(ma)
  # The vector for the simulation result
  y <- numeric(n)
  # Generate the random normal values
  eps <- rnorm(n)
  # Run the simulation
  for(i in (max(p,q)+1):n){
    y[i] <- eps[i] + sum(y[i-(1:p)] * ar) + sum(eps[i-(1:q)] * ma)
  }
  # Return without the burn-in period
  return(y[(nburnin+1):n])
}


## ---------------------------------------------------------------------------------------
## 1.9. Redo plots of simulations and ACFs for the following ϕ1-values. 
## ϕ1 = −0.8

# Initialize 
phi1 <- -0.8
ar_data2 <- data.frame( time = 1:n )
acf_data2 <- data.frame( time = 0:nlag, 
                         rho = rep(0, nlag+1) )

# Base cases of the recursive formula
acf_data2$rho[1] <- 1
acf_data2$rho[2] <- -phi1 / (1+phi2)

# Calculate recursively ρ(k)
for (i in 3:(nlag+1)){
  acf_data2$rho[i] <- -phi1*acf_data2$rho[i-1] - phi2*acf_data2$rho[i-2] 
}

# AR(2) process
model <- list(ar=c(-phi1, -phi2))

# Simulate 5 realizations
for (i in 1:5) {
  col_name <- paste0("Realization ", i)  
  ar_data2[col_name] <- rep(0, n)  
  
  (seed_name <- paste0(i,i+1,i+2))    # set seed for simulation of process
  set.seed(seed_name)
  ar_data2[col_name] <- sim(model, n) # simulate a non-stationnary AR process
}


# Plot the realizations
matplot(ar_data2$time, ar_data2[, -1], type = "l", lty = 1, col = 1:5, xlab = "Time", ylab = "Value",
        main = "Simulated processes for ϕ1 = −0.8", ylim = range(ar_data2[, -1]))
legend("topright", legend = colnames(ar_data[, -1]), col = 1:5, lty = 1, cex = 0.8)


# Calculate the empirical autocorrelation for each realization
for (i in 1:5) {
  col_name <- paste0("Realization ", i)
  acf_data2[col_name] <- acf(ar_data2[col_name], lag.max = nlag, plot = FALSE)$acf
}

# Plot the auto correlations on a line plot
matplot(acf_data2$time, acf_data2[,-1], type = c("h", rep("l", 5)), lty = c(1,rep(1,5)), lwd = c(2,rep(1,5)), col = 1:5, xlab = "Time", ylab = "Value",
        main = "Autocorrelation Plot for ϕ1 = −0.8", ylim = range(acf_data2[, -1]))
legend("topright", legend = c("Theoretical ACF", paste(colnames(acf_data[,-1][,-1]),"ACF")), col = 1:5, lty = c(1,rep(1,5)), lwd = c(2,rep(1,5)), cex = 0.8)



## ---------------------------------------------------------------------------------------
## 1.10. Redo plots of simulations and ACFs for the following ϕ1-values. 
## ϕ1 = −0.85

phi1 <- -0.85
ar_data2 <- data.frame( time = 1:n )
acf_data2 <- data.frame( time = 0:nlag, 
                         rho = rep(0, nlag+1) )

# Base cases of the recursive formula
acf_data2$rho[1] <- 1
acf_data2$rho[2] <- -phi1 / (1+phi2)

# Calculate recursively ρ(k)
for (i in 3:(nlag+1)){
  acf_data2$rho[i] <- -phi1*acf_data2$rho[i-1] - phi2*acf_data2$rho[i-2] 
}

# AR(2) process
model <- list(ar=c(-phi1, -phi2))

# Simulate 5 realizations
for (i in 1:5) {
  col_name <- paste0("Realization ", i)  
  ar_data2[col_name] <- rep(0, n)  
  
  (seed_name <- paste0(i,i+1,i+2))    # set seed for simulation of process
  set.seed(seed_name)
  ar_data2[col_name] <- sim(model, n) # simulate a non-stationary AR process
}

# Plot the realizations
matplot(ar_data2$time, ar_data2[, -1], type = "l", lty = 1, col = 1:5, xlab = "Time", ylab = "Value",
        main = "Simulated processes for ϕ1 = −0.85", ylim = range(ar_data2[, -1]))
legend("topright", legend = colnames(ar_data[, -1]), col = 1:5, lty = 1, cex = 0.8)


# Calculate the empirical autocorrelation for each realization
for (i in 1:5) {
  col_name <- paste0("Realization ", i)
  acf_data2[col_name] <- acf(ar_data2[col_name], lag.max = nlag, plot = FALSE)$acf
}


# Plot the auto correlations on a line plot
matplot(acf_data2$time, acf_data2[,-1], type = c("h", rep("l", 5)), lty = c(1,rep(1,5)), lwd = c(2,rep(1,5)), col = 1:5, xlab = "Time", ylab = "Value",
        main = "Autocorrelation Plot for ϕ1 = −0.85", ylim = range(acf_data2[, -1]))
legend("topright", legend = c("Theoretical ACF", paste(colnames(acf_data[,-1][,-1]),"ACF")), col = 1:5, lty = c(1,rep(1,5)), lwd = c(2,rep(1,5)), cex = 0.8)


