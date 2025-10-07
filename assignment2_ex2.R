# load excel read library
library(utils)
library(ggplot2)
library(xtable)

# clear variables
rm(list=ls())

# resizing the graphics window
par(mfrow=c(1,1))

# import Excel file into R
project_data <- read.csv('~/Cours DTU/2024 SPRING/02417 Time Series Analysis/Assignment 2/datasolar.csv') # time series

# get time axis
project_data$time <- project_data$year +  (project_data$month-1)/12
project_data <- data.frame(year = project_data$year, 
                           month = project_data$month, 
                           time = project_data$time,
                           power = project_data$power)

# plot the time series
first_year <- as.integer(project_data$year[1])
last_year <- as.integer(project_data$year[length(project_data$year)])
plot(project_data$time, project_data$power, type = "b", 
     xaxp = c(first_year, last_year+1, last_year+1 - first_year),
     xlab = "Time", ylab = "Generation (MWh)")



## ---------------------------------------------------------------------------------------
## Part 2 - Predicting monthly solar power
## 2.1. Introduce Xt = log(Yt)−µ and re-write the model to calculate the residuals εt+1|t. 
## Do a model validation by checking the assumptions of i.i.d. errors

# Seasonal AR model parameters
phi1 <- -0.38
seas_phi1 <- -0.94
mu <- 5.72

sigma_eps <- 0.22

# Store the observations, predictions and residuals of the time series x_t
obs_data <- data.frame(
  time = project_data$time,
  Xt = log(project_data$power) - mu,
  Xt_hat = rep(NA, length(project_data$time)),
  res = rep(NA, length(project_data$time))
)

# Recursively compute the predictions and their associated residuals
for (i in 14:length(obs_data$Xt)){
  obs_data$Xt_hat[i] <- - phi1 * obs_data$Xt[i-1] - seas_phi1 * obs_data$Xt[i-12] - phi1*seas_phi1 * obs_data$Xt[i-13]
  obs_data$res[i] <- obs_data$Xt[i] - obs_data$Xt_hat[i]
}

# Plot the predictions of our model
ggplot(obs_data, aes(x=time, y=Xt)) +
  geom_line(data=obs_data, aes(y=Xt, col="observations")) +
  geom_point(data=obs_data, aes(y=Xt, col="observations"), size=2) +
  geom_line(data=obs_data, aes(y=Xt_hat, col="predictions")) +
  geom_point(data=obs_data, aes(y=Xt_hat, col="predictions"), size=2) +
  xlim(2008, 2011) +
  labs(x = "Time", y = "Value", color="Data source") +
  ggtitle(paste0("One-step predictions for the time series Xt"))+
  theme_minimal() +
  scale_color_manual(values = c("black", "blue"), 
                     labels = c("observations", "predictions"))


# Plot residuals
first_year <- as.integer(project_data$year[13])
last_year <- as.integer(project_data$year[length(obs_data$time)])+1
plot(obs_data$time[13:length(obs_data$time)], obs_data$res[13:length(obs_data$time)], 
     xlim = c(first_year, last_year), xaxp = c(first_year, last_year, 2),
     xlab="Time", ylab="Residuals", main="Series of residuals")

# Inspect residuals
acf(obs_data$res[14:length(obs_data$time)], main="Series of residuals")

qqnorm(obs_data$res[14:length(obs_data$time)])
qqline(obs_data$res[14:length(obs_data$time)])


## ---------------------------------------------------------------------------------------
## 2.2. Calculate Yt+k|t for t = 36 and k = 1,...,12, i.e. predict the power for the following twelve months. 

# Store the predicted values of Xt
pred_data <- data.frame(
  time = obs_data$time,
  Xt = obs_data$Xt
)

pred_data <- rbind(pred_data, data.frame(time = seq(2011, 2011 + 11/12, by = 1/12),
                                         Xt = rep(0, 12)))

# Recursively compute the predicted values for k-step predictions
for (i in 37:48){
  pred_data$Xt[i] <- - phi1 * pred_data$Xt[i-1] - seas_phi1 * pred_data$Xt[i-12] - phi1*seas_phi1 * pred_data$Xt[i-13]
}


# Compute the associated time series Yt
pred_data$Yt = exp(pred_data$Xt + mu)


# Create LaTeX table code using xtable
latex_data <- data.frame(
  k = 1:12,
  predictions = pred_data$Yt[37:48]
)
latex_code <- xtable(latex_data, digits = 2)

# Print the LaTeX code
print(latex_code, include.rownames = FALSE, only.contents = TRUE)


# Plot the predictions of our model
latex_data$time <- pred_data$time[37:48]

new_line <- data.frame(k=0, time=latex_data$time[1]-1/12, predictions=pred_data$Yt[36])
latex_data <- rbind(new_line, latex_data)

ggplot() +
  geom_line(data = latex_data, aes(x = time, y = predictions, color = "predictions")) +
  geom_point(data = latex_data, aes(x = time, y = predictions, color = "predictions"), size = 2) +
  
  geom_line(data=project_data, aes(x=time, y=power, col="observations")) +
  geom_point(data=project_data, aes(x=time, y=power, col="observations"), size=2) +
  
  xlim(2008, 2012) +
  labs(x = "Time", y = "Generation (MWh)", color="Data source") +
  ggtitle(paste0("K-step predictions for our seasonal AR model")) +
  theme_minimal() +
  scale_color_manual(values = c("black", "blue"), 
                     labels = c("observations", "predictions"))
  
  


## ---------------------------------------------------------------------------------------
## 2.3. Calculate 95% prediction intervals for the twelve months ahead and add them to the plot.

k <- 12
var_eps <- sigma_eps^2

# Initialize the new columns of our dataframe
latex_data$Xt <- pred_data$Xt[36:48]
latex_data$variance_X <- rep(NA, k+1)
latex_data$upper_X <- rep(NA, k+1)
latex_data$lower_X <- rep(NA, k+1)


# Initialize our computed values
i <- 1
sum_var <- phi1^(0) 

latex_data$variance_X[i+1] <- sum_var * var_eps
latex_data$upper_X[i+1] <- latex_data$Xt[i+1] + qnorm(0.975) * sqrt(latex_data$variance_X[i+1])
latex_data$lower_X[i+1] <- latex_data$Xt[i+1] - qnorm(0.975) * sqrt(latex_data$variance_X[i+1])


# X_t time series: compute the variance of our prediction errors, 
# the lower and upper limits of our prediction intervals
for (i in 2:k){
  sum_var <- sum_var + phi1^(2*(i-1)) 
  
  latex_data$variance_X[i+1] <- sum_var * var_eps
  latex_data$upper_X[i+1] <- latex_data$Xt[i+1] + qnorm(0.975) * sqrt(latex_data$variance_X[i+1])
  latex_data$lower_X[i+1] <- latex_data$Xt[i+1] - qnorm(0.975) * sqrt(latex_data$variance_X[i+1])
}


# Associate the correspondent Y_t prediction intervals
latex_data$upper_Y <- exp(latex_data$upper_X + mu)
latex_data$lower_Y <- exp(latex_data$lower_X + mu)


# Plot the resulted prediction intervals
bleu_clair <- "#ADD8E6"

ggplot() +
  geom_line(data = latex_data, aes(x = time, y = predictions, color = "predictions")) +
  geom_point(data = latex_data, aes(x = time, y = predictions, color = "predictions"), size = 2) +
  
  geom_ribbon(data = latex_data, aes(x = time, ymin = lower_Y, ymax = upper_Y, fill = "predictions intervals"), alpha = 0.2) +
  
  geom_line(data=project_data, aes(x=time, y=power, col="observations")) +
  geom_point(data=project_data, aes(x=time, y=power, col="observations"), size=2) +
  
  xlim(2008, 2012) +
  labs(x = "Time", y = "Generation (MWh)", color="Data source", fill=NULL) +
  ggtitle(paste0("K-step predictions for our seasonal AR model")) +
  theme_minimal() +
  scale_color_manual(values = c("black", "blue"), 
                     labels = c("observations", "predictions")) +
  scale_fill_manual(values = "blue", labels = "prediction intervals") 
  

# Create LaTeX table code using xtable
latex_data <- latex_data[-1,]
latex_data_2 <- data.frame(
  k = format(latex_data$k, nsmall=0),
  time = round(latex_data$time, digits=2),
  predicted_value = round(latex_data$predictions, digits=2),
  prediction_interval = paste("[", round(latex_data$lower_Y, digits=2), "; ", round(latex_data$upper_Y, digits=2), "]")
)

latex_code_2 <- xtable(latex_data_2)

# Print the LaTeX code
print(latex_code_2, include.rownames = FALSE, only.contents = TRUE)


