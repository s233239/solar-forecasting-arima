# load excel read library
library(utils)
library(ggplot2)
library(xtable)

# clear variables
rm(list=ls())

# resizing the graphics window
par(mfrow=c(1,1))



## ---------------------------------------------------------------------------------------
## Part 3 - Simulating seasonal processes
## Simulate the following models. 
## Plot the simulations and the associated autocorrelation functions (ACF and PACF).

# Function for plot
plotit <- function(x, tit=NA){
  layout(rbind(1,2:3))
  par(mar=c(3,3,1,1), mgp=c(2, 0.7,0))
  plot(x, type="l", ylab="X", xlab="Time")
  title(main=tit)
  acf(x, lag.max=50, lwd=2)
  pacf(x, lag.max=50, lwd=2)
}


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

# Set a fixed seed for all of our simulations
set.seed(42)


## ---------------------------------------------------------------------------------------
## 3.1. A (1,0,0)×(0,0,0)12 model with the parameter ϕ1 = 0.6

# AR(1) process
phi <- c(0.6)
model <- list(ar=-phi)

# Number of observations of our simulation, high enough to not have a lot of noise
n <- 1000

# Simulate 1 realization and plot it with its ACFs

plotit( arima.sim(model, n), "A (1,0,0)×(0,0,0)12 with ϕ1 = 0.6" )     
#plotit( sim(model, n), "A (1,0,0)×(0,0,0)12 with ϕ1 = 0.6" )     

############
# Plot the autocorrelation function (theory)
ar1_data <- data.frame(
  lag = 0:50
)

ar1_data$acf <- (-phi)^(ar1_data$lag)

par(mfrow=c(1,1))
acf(arima.sim(model, n))
lines(ar1_data$lag ,ar1_data$acf, type="l",col="red", lty=2, lwd=2)



## ---------------------------------------------------------------------------------------
## 3.2. A (0,0,0)×(1,0,0)12 model with the parameter Φ1 = −0.9

# AR(12) process with Φi = 0 for i=1,...11 and Φ12 = −0.9
phi <- c(rep(0,11),-0.9)
model <- list(ar=-phi)

# Number of observations of our simulation, high enough to not have a lot of noise
n <- 1000

# Simulate 1 realization and plot it with its ACFs

plotit( arima.sim(model, n), "A (0,0,0)×(1,0,0)12 with Φ1 = −0.9" )     
#plotit( sim(model, n), "A (0,0,0)×(1,0,0)12 with Φ1 = −0.9" ) 

############
# Plot the autocorrelation function (theory)
ar12_data <- data.frame(
  lag = 0:50,
  acf = rep(0,51)
)

for (k in 0:(as.integer(50/12))){
  ar12_data$acf[12*k+1] <- (-phi[12])^k
}

par(mfrow=c(1,1))
acf(arima.sim(model, n))
lines(ar12_data$lag ,ar12_data$acf, type="l",col="red", lty=2, lwd=2)



## ---------------------------------------------------------------------------------------
## 3.3. A (1,0,0)×(0,0,1)12 model with the parameters ϕ1 = 0.9 and Θ1 = −0.7

# ARMA(1,12) process with ϕ1 = 0.9, θi = 0 for i=1,...11 and θ12 = −0.7
phi <- c(0.9)
theta <- c(rep(0,11),-0.7)
model <- list(ar=-phi, ma=theta)

# Number of observations of our simulation, high enough to not have a lot of noise
n <- 1000

# Simulate 1 realization and plot it with its ACFs

plotit( arima.sim(model, n), "A (1,0,0)×(0,0,1)12 with ϕ1 = 0.9 and Θ1 = −0.7" )     
#plotit( sim(model, n), "A (1,0,0)×(0,0,1)12 with ϕ1 = 0.9 and Θ1 = −0.7" ) 



## ---------------------------------------------------------------------------------------
## 3.4. A (1,0,0)×(1,0,0)12 model with the parameters ϕ1 = −0.6 and Φ1 = −0.8

# AR(13) process with ϕ1 = -0.6, ϕ12 = -0.8, ϕ13 = ϕ1*ϕ12 and ϕi = 0 for i=2,...11
phi1 <- -0.6
phi12 <- -0.8
phi <- c(phi1, rep(0,10), phi12, phi1*phi12)
model <- list(ar=-phi)

# Number of observations of our simulation, high enough to not have a lot of noise
n <- 10000

# Simulate 1 realization and plot it with its ACFs

plotit( arima.sim(model, n), "A (1,0,0)×(1,0,0)12 with ϕ1 = −0.6 and Φ1 = −0.8" )     
#plotit( sim(model, n), "A (1,0,0)×(1,0,0)12 with ϕ1 = −0.6 and Φ1 = −0.8" ) 



## ---------------------------------------------------------------------------------------
## 3.5. A (0,0,1) ×(0,0,1)12 model with the parameters θ1 = 0.4 and Θ1 = −0.8

# MA(13) process with θ1 = -0.6, θ12 = -0.8, θ13 = θ1*θ12 and θi = 0 for i=2,...11
theta1 <- 0.4
theta12 <- -0.8
theta <- c(theta1, rep(0,10), theta12, theta1*theta12)
model <- list(ma=theta)

# Number of observations of our simulation, high enough to not have a lot of noise
n <- 1000

# Simulate 1 realization and plot it with its ACFs

plotit( arima.sim(model, n), "A (0,0,1) ×(0,0,1)12 with θ1 = 0.4 and Θ1 = −0.8" )     
#plotit( sim(model, n), "A (0,0,1) ×(0,0,1)12 with θ1 = 0.4 and Θ1 = −0.8" ) 



## ---------------------------------------------------------------------------------------
## 3.6. A (0,0,1) ×(1,0,0)12 model with the parameters θ1 = −0.4 and Φ1 = 0.7

# ARMA(12,1) process with ϕ12 = 0.7, θ1 = −0.4 and ϕi = 0 for i=1,...11
phi <- c(rep(0,11), 0.7)
theta <- -0.4
model <- list(ar=-phi, ma=theta)

# Number of observations of our simulation, high enough to not have a lot of noise
n <- 1000

# Simulate 1 realization and plot it with its ACFs

plotit( arima.sim(model, n), "A (0,0,1) ×(1,0,0)12 with θ1 = −0.4 and Φ1 = 0.7" )     
#plotit( sim(model, n), "A (0,0,1) ×(1,0,0)12 with θ1 = −0.4 and Φ1 = 0.7" ) 




