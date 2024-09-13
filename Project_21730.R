{
  # install.packages("tseries")
  # install.packages("car")
  # install.packages("rugarch")
  # install.packages("ggplot2")
  # install.packages("qqplotr")
  # install.packages("reshape2")
  # library(tseries)
  # library(car)
  # library(lubridate)
  # library(zoo)
  # library(ggplot2)
  # library(qqplotr)
  # library(rugarch)
  # library(reshape2)
  # library(tidyr)
}

rm(list = ls())
graphics.off()

{data <- read.csv("TESLA.csv")
  
  # Adjust Prices to StockSplits
  data$Unadjusted_PRC<-data$PRC
  data$Adjusted_PRC<-data$PRC/data$CFACPR 
  
  # Create Prices Data Frame
  PRC <- dcast(data, date ~ PERMNO, value.var = "Adjusted_PRC")
  names(PRC) <- c("date", "TESLA")
  
  # Change Date type
  PRC$int_date <- PRC$date
  PRC$date <- ymd(PRC$date)
  
  # Create Returns Data Frame
  RET <- dcast(data, date ~ PERMNO, value.var = "RET")
  names(RET) <- c("date", "TESLA")
  
  # Calculate Log Returns
  LOG_RET <- log(1 + RET[2])
  
  LOG_RET$date <- RET$date
  LOG_RET$int_date <- LOG_RET$date
  LOG_RET$date <- ymd(LOG_RET$date)
  
  save(LOG_RET, file = "LOG_RET.RData")
  save(PRC, file = "PRC.RData") 
}

load("LOG_RET.RData")
load("PRC.RData")

y <- LOG_RET$TESLA

portfolio_value <- 1000



# Plot histogram
ggplot(data.frame(Portfolio_y), aes(x = y)) +
  geom_histogram(binwidth = 0.02, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = -VaR_true, linetype = "dashed", color = "red") +
  labs(title = "Histogram of Returns",
       x = "Returns", y = "Frequency") +
  theme_minimal()
  

  
# Put results in a list

Results = list()
# GARCH(1,1) ------

spec.1 = ugarchspec(
  variance.model = list(garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0),
                    include.mean = FALSE),
  distribution.model = "norm" 
)
Results$GARCH11 = ugarchfit(spec = spec.1, data = y)


Results$GARCH11@fit$matcoef

theta <- Results$GARCH11@fit$matcoef
p <- 0.05

# Statistics ----

#Check DoF function
check_normality <- function(returns) {
  # Get the mean and sd of returns
  mean_returns <- mean(returns)
  sd_returns <- sd(returns)
  
  # Display mean and standard deviation
  cat("Mean:", round(mean_returns, 3), "\n")
  cat("Standard Deviation:", round(sd_returns, 3), "\n")
  
  # Create the histogram
  hist(returns, freq = FALSE, main = paste("Returns of TESLA"), col = "skyblue", breaks = 50)
  
  # Add the normal distribution
  x <- seq(-3, 3, 0.001)
  lines(x, dnorm(x, mean = mean_returns, sd = sd_returns), lwd = 2, col = "red")
  
  # Test for normality
  jb_test <- jarque.bera.test(returns)
  cat("Jarque-Bera Test p-value:", jb_test$p.value, "\n")

  # qqPlot of the normal distribution
  qqPlot(returns, distribution = "norm", envelope = FALSE, main = "Returns Q-Q Plot")
  
  # Q-Q Plots for t-distributions with different degrees of freedom
  for (df in c(2, 3, 4)) {
    qqPlot(returns, distribution = "t", df = df, envelope = FALSE, main = paste(df, "Degrees of Freedom"))
  }
}



# Run normality function
check_normality(y)


# Simulations --------

S <- 1000
T_values <- c(250, 1000, 2500, 5000)  # Adjust this range as needed

# Create vectors to hold the fitted parameters for different sample sizes
omega_values <- alpha_values <- beta_values <- nu_values <- vector("list", length = length(T_values))

# Run simulations for different sample sizes
for (i in seq_along(T_values)) {
  T <- T_values[i]
  cat("Simulating for T =", T, "\n")
  
  # Create vectors to hold the fitted parameters
  omega_sim <- alpha_sim <- beta_sim <- nu_sim <- vector("numeric", length = S)
  
  # Simulation 
  simulations <- ugarchsim(Results$GARCH11, m.sim = S, n.sim = T)
  
  # Fit a tGARCH for every simulation and store the parameter values
  for (j in 1:S) {
    cat("Fitting simulation:", j, "\n")
    
    # Use tryCatch to handle errors
    tryCatch({
      # Fit the GARCH model
      temp <- ugarchfit(spec = spec.1, data = simulations@simulation$seriesSim[, j])
      
      # Add parameters to vectors created
      omega_sim[j] <- unname(coef(temp)['omega'])
      alpha_sim[j] <- unname(coef(temp)['alpha1'])
      beta_sim[j] <- unname(coef(temp)['beta1'])
    }, error = function(e) {
      cat("Error in simulation:", j, "- Skipping\n")
      omega_sim[j] <- NA
      alpha_sim[j] <- NA
      beta_sim[j] <- NA
    })
  }
  
  # Update the vectors with parameter values
  omega_values[[i]] <- omega_sim
  alpha_values[[i]] <- alpha_sim
  beta_values[[i]] <- beta_sim
}
alpha_values <- Background.2_results[["alpha_values"]]
omega_values <- Background.2_results[["omega_values"]]
beta_values <- Background.2_results[["beta_values"]]



# GARCH parameters distribution ----- ----

# Create a data frame for ggplot
param_df <- data.frame(omega = omega_values[[4]],
                       alpha = alpha_values[[4]],
                       beta = beta_values[[4]]       
)

# OMEGA

ggplot(param_df, aes(x = omega)) +
  geom_density(fill = "skyblue", alpha = 0.5, linewidth = 0) +
  geom_vline(xintercept = theta[1], color = "blue", linetype = "dashed", linewidth = 0.3) +
  annotate("text", x = theta[1], y = 0, label = paste(round(theta[1], 8)), vjust = 1.2, hjust= 1.1, color = "blue", size = 3) +
  labs(x = "Omega Value", y = "Density") +
  theme_minimal()



# ALPHA

ggplot(param_df, aes(x = alpha)) +
  geom_density(fill = "coral", alpha = 0.5, linewidth = 0) +
  geom_vline(xintercept = theta[2], color = "blue", linetype = "dashed", size = 0.5) +
  annotate("text", x = theta[2], y = 0, label = paste(round(theta[2], 3)), vjust = 1.2, hjust = -0.1, color = "blue", size = 3) +
  labs(x = "Alpha Value", y = "Density") +
  theme_minimal()

# BETA

ggplot(param_df, aes(x = beta)) +
  geom_density(fill = "olivedrab3", alpha = 0.5, linewidth = 0) +
  geom_vline(xintercept = theta[3], color = "blue", linetype = "dashed", size = 0.5) +
  annotate("text", x = theta[3], y = 0, label = paste(round(theta[3], 3)), vjust = 1.2, hjust = -0.1, color = "blue", size = 3) +
  labs(x = "Beta Value", y = "Density") +
  theme_minimal()

# Scatter Plot - Parameters ----

# OMEGA ----

omega_df <- data.frame(A_250 = omega_values[[1]],
                       B_1000 = omega_values[[2]],
                       C_2500 = omega_values[[3]],
                       E_5000 = omega_values[[4]]
)

omega_df_long <- gather(omega_df, key = "Sample_Size", value = "Omega", -c())

# Specific point for theta[1, 1]
theta_point <- data.frame(x = 3, y = theta[1, 1], label = "Theta[1, 1]")

ggplot(omega_df_long, aes(x = Sample_Size, y = Omega)) +
  geom_point() +
  geom_hline(yintercept = theta[1, 1], linetype = "dashed", color = "red", linewidth = 0.4) + # Add red line
    labs(title = "Scatter Plot - Omega Values",
       x = "Sample Size",
       y = "Omega Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels= c("250", "1000", "2500", "5000"))
  
  


# Create a scatter plot for specific sample size
ggplot(omega_df, aes(x = seq_along(E_5000), y = E_5000)) +
  geom_point() +
  geom_point(data = theta_point, aes(x = x, y = y), color = "red", size = 3) +
  # geom_ribbon(aes(ymin = min(E_5000), ymax = max(E_5000)), fill = "blue", alpha = 0.3) +  # Add confidence interval ribbon
  geom_text(data = theta_point, aes(x = x, y = y, label = label), vjust = 1, hjust = 1, size = 3, color = "red") +
  labs(title = "Scatter Plot - Omega_5000",
       x = "Observation Index",
       y = "Omega Values") +
  theme_minimal()

# ALPHA ----

alpha_df <- data.frame(A_250 = alpha_values[[1]],
                       B_1000 = alpha_values[[2]],
                       C_2500 = alpha_values[[3]],
                       E_5000 = alpha_values[[4]]
)

alpha_df_long <- gather(alpha_df, key = "Sample_Size", value = "Alpha", -c())

# Specific point for theta[1, 1]
theta_point <- data.frame(x = 3, y = theta[2, 1], label = "Theta[2, 1]")

ggplot(alpha_df_long, aes(x = Sample_Size, y = Alpha)) +
  geom_point() +
  geom_hline(yintercept = theta[2, 1], linetype = "dashed", color = "red", linewidth = 0.4) + # Add red line
  labs(title = "Scatter Plot - Alpha Values",
       x = "Sample Size",
       y = "Alpha Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels= c("250", "1000", "2500", "5000"))



# Create a scatter plot for specific sample size
ggplot(alpha_df, aes(x = seq_along(E_5000), y = E_5000)) +
  geom_point() +
  geom_point(data = theta_point, aes(x = x, y = y), color = "red", size = 3) +
  geom_text(data = theta_point, aes(x = x, y = y, label = label), vjust = -0.5, hjust = 0, size = 3, color = "red") +
  labs(title = "Scatter Plot - Alpha_5000",
       x = "Observation Index",
       y = "Alpha Values") +
  theme_minimal()


# BETA ----

beta_df <- data.frame(A_250 = beta_values[[1]],
                       B_1000 = beta_values[[2]],
                       C_2500 = beta_values[[3]],
                       E_5000 = beta_values[[4]]
)

beta_df_long <- gather(beta_df, key = "Sample_Size", value = "Beta", -c())


# Specific point for theta[1, 1]
theta_point <- data.frame(x = 3, y = theta[3, 1], label = "Theta[3, 1]")

ggplot(beta_df_long, aes(x = Sample_Size, y = Beta)) +
  geom_point() +
  geom_hline(yintercept = theta[3, 1], linetype = "dashed", color = "red", linewidth = 0.4) + # Add red line
  labs(title = "Scatter Plot - Beta Values",
       x = "Sample Size",
       y = "Beta Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels= c("250", "1000", "2500", "5000"))



# Create a scatter plot for specific sample size
ggplot(beta_df, aes(x = seq_along(E_5000), y = E_5000)) +
  geom_point() +
  geom_point(data = theta_point, aes(x = x, y = y), color = "red", size = 3) +
  geom_text(data = theta_point, aes(x = x, y = y, label = label), vjust = 1, hjust = 1, size = 3, color = "red") +
  labs(title = "Scatter Plot - Beta_5000",
       x = "Observation Index",
       y = "Beta Values") +
  theme_minimal()


# Scatter Plot- VaR -----

library(dplyr)

# Create an empty list to store VaR values
var_list <- list()

# Loop over the sample sizes
for (i in seq_along(T_values)) {
  T <- T_values[i]
  cat("Calculating VaR for T =", T, "\n")
  
  # Extract the simulated parameters for the current sample size
  omega_sim <- omega_values[[i]]
  alpha_sim <- alpha_values[[i]]
  beta_sim <- beta_values[[i]]
  
  # Create a data frame for the current sample size
  df <- data.frame(Sample_Size = as.factor(T))
  
  # Create a list to store VaR values for the current sample size
  var_values <- list()
  
  # Loop over the simulations
  for (j in 1:S) {
    tryCatch({
      # Calculate VaR for each simulation
      s2_sim <- omega_sim[j] + alpha_sim[j] * tail(y, 1)^2 + beta_sim[j] * tail(Results$GARCH11@fit$var, 1)
      VaR_sim <- -qnorm(p, sd = sqrt(s2_sim))*portfolio_value
      
      # Add VaR value to the list
      var_values[[j]] <- VaR_sim
    }, error = function(e) {
      cat("Error in simulation:", j, "- Skipping\n")
    })
  }
  
  # Add the list of VaR values to the var_list
  var_list[[i]] <- var_values
}

# Combine the list of VaR values into a single data frame
final_df <- data.frame(do.call(cbind, var_list))

# class(final_df)

colnames(final_df) <- c("A_250", "B_1000", "C_2500", "E_5000")

final_df$A_250 <- unlist(final_df$A_250)
final_df$B_1000 <- unlist(final_df$B_1000)
final_df$C_2500 <- unlist(final_df$C_2500)
final_df$E_5000 <- unlist(final_df$E_5000)

# Convert the data frame to long format for ggplot
final_df_long <- gather(final_df, key = "Sample_Size", value = "VaR")

final_df_long$VaR <- unlist(final_df_long$VaR)

# str(final_df_long$VaR)

s2_true <- theta[1,1] + theta[2,1] * tail(y, 1)^2 + theta[3,1] * tail(Results$GARCH11@fit$var, 1)
VaR_true <- -qnorm(p, sd = sqrt(s2_sim))*portfolio_value

# Create a scatter plot for each sample size
ggplot(final_df_long, aes(x = Sample_Size, y = VaR)) +
  geom_point() +
  geom_hline(yintercept = VaR_true, linetype = "dashed", color = "red", linewidth = 0.4) + # Add red line
  labs(title = "Scatter Plot - VaR for Different Sample Sizes",
       x = "Simulation Index",
       y = "VaR Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels= c("250", "1000", "2500", "5000"))

#Scatterplot for single Sample Size

VaR_point <- data.frame(x = 3, y = VaR_true, label = "VaR True")


ggplot(final_df, aes(x = seq_along(E_5000), y = E_5000)) +
  geom_point() +
  geom_point(data = VaR_point, aes(x = x, y = y), color = "red", size = 3) +
  geom_text(data = VaR_point, aes(x = x, y = y, label = label), vjust = 1, hjust = 1, size = 3, color = "red") +
   labs(title = "Scatter Plot - VaR_5000",
       x = "Observation Index",
       y = "VaR Values") +
  theme_minimal()





# Learning Curves GARCH ----
T_values <- c(250, 1000, 2500, 5000)

# Initialize vectors to store MSE values for each parameter
mse_omega <- mse_alpha <- mse_beta <- mse_var<- vector("numeric", length = length(T_values))

# Set the true parameter values
true_omega <- theta[1,1]  
true_alpha <- theta[2,1]  
true_beta <- theta[3,1]  
true_VaR <- VaR_true

# Run analyses for different sample sizes
for (i in seq_along(T_values)) {
  T <- T_values[i]
  cat("Analyzing for T =", T, "\n")
  
  # Calculate MSE for each parameter Mean squared Error 
  mse_omega[i] <- mean((omega_values[[i]] - true_omega)^2, na.rm = TRUE)
  mse_alpha[i] <- mean((alpha_values[[i]]- true_alpha)^2, na.rm = TRUE)
  mse_beta[i] <- mean((beta_values[[i]] - true_beta)^2, na.rm = TRUE)
  mse_var[i] = mean((final_df[[i]] - true_VaR)^2, na.rm = TRUE)
}

# Plot learning curves

plot(T_values, mse_omega, type = 'o', col = 'blue',
     xlab = 'Sample Size (T)', ylab = 'MSE(omega)',
     main = 'Learning Curve for omega')

plot(T_values, mse_alpha, type = 'o', col = 'green',
     xlab = 'Sample Size (T)', ylab = 'MSE(alpha)',
     main = 'Learning Curve for alpha')

plot(T_values, mse_beta, type = 'o', col = 'red',
     xlab = 'Sample Size (T)', ylab = 'MSE(beta)',
     main = 'Learning Curve for beta')

plot(T_values, mse_var, type = 'o', col = 'orange',
     xlab = 'Sample Size (T)', ylab = 'MSE(VaR)',
     main = 'Learning Curve for VaR')


# Convergence Plots GARCH ----

S_2 <- 2
T_values_2 <- seq(100, 10100, 1000)  # Adjust this range as needed

# Create vectors to hold the fitted parameters for different sample sizes
omega_values_2 <- alpha_values_2 <- beta_values_2 <- nu_values_2 <- vector("list", length = length(T_values_2))

# Run simulations for different sample sizes
for (i in seq_along(T_values_2)) {
  T <- T_values_2[i]
  cat("Simulating for T =", T, "\n")
  
  # Create vectors to hold the fitted parameters
  omega_sim_2 <- alpha_sim_2 <- beta_sim_2 <- nu_sim_2 <- vector("numeric", length = S_2)
  
  # Simulation 
  simulations_2 <- ugarchsim(Results$GARCH11, m.sim = S_2, n.sim = T)
  
  # Fit a tGARCH for every simulation and store the parameter values
  for (j in 1:S_2) {
    cat("Fitting simulation:", j, "\n")
    
    # Use tryCatch to handle errors
    tryCatch({
      # Fit the GARCH model
      temp <- ugarchfit(spec = spec.1, data = simulations@simulation$seriesSim[, j])
      
      # Add parameters to vectors created
      omega_sim_2[j] <- unname(coef(temp)['omega'])
      alpha_sim_2[j] <- unname(coef(temp)['alpha1'])
      beta_sim_2[j] <- unname(coef(temp)['beta1'])
    }, error = function(e) {
      cat("Error in simulation:", j, "- Skipping\n")
      omega_sim_2[j] <- NA
      alpha_sim_2[j] <- NA
      beta_sim_2[j] <- NA
    })
  }
  
  # Update the vectors with parameter values
  omega_values_2[[i]] <- omega_sim_2
  alpha_values_2[[i]] <- alpha_sim_2
  beta_values_2[[i]] <- beta_sim_2
}
alpha_values_2 <- Background_project_results[["alpha_values_2"]]
omega_values_2 <- Background_project_results[["omega_values_2"]]
beta_values_2 <- Background_project_results[["beta_values_2"]]




plotParameterSmooth <- function(parameter_values_list, parameter_name, T_val, theta_value) {
  # Calculate the mean for each sample size
  mean_values <- sapply(parameter_values_list, function(param) mean(param, na.rm = TRUE))
  
  # Smooth the mean values
  smooth_values <- loess(mean_values ~ T_val)$fitted
  
  # Create a data frame for ggplot
  data <- data.frame(T = T_val, Mean = mean_values, Smooth = smooth_values)
  
  # Plotting with ggplot
  ggplot(data, aes(x = T)) +
    geom_line(aes(y = Mean), color = "black") +
    geom_hline(yintercept = theta_value, linetype = "dashed", color = "red") +
    labs(title = paste("Mean of", parameter_name, "vs. Sample Size"),
         x = "Sample Size", y = paste("Mean of", parameter_name)) +
    theme_minimal()
}

# Plot mean for omega with smoothed line as simulation horizon T increases
plotParameterSmooth(omega_values_2, "omega", T_values_2, theta[1,1])
plotParameterSmooth(alpha_values_2, "alpha", T_values_2, theta[2,1])
plotParameterSmooth(beta_values_2, "beta", T_values_2, theta[3,1])





#--------
# tGARCH(1,1) ----


spec.2 = ugarchspec(
  variance.model = list(garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), 
                    include.mean = FALSE),
  distribution.model = "std"
)
Results$tGARCH11 = ugarchfit(spec = spec.2, data = y)

theta_t <- Results$tGARCH11@fit$matcoef

# Simulation ----
S <- 1000
T_whole <- length(y)
T_values <- c(250, 1000, 2500, 5000)  # Adjust this range as needed

# Create vectors to hold the fitted parameters for different sample sizes
omega_values_t <- alpha_values_t <- beta_values_t <- nu_values_t <- vector("list", length = length(T_values))

# Run simulations for different sample sizes
for (i in seq_along(T_values)) {
  T <- T_values[i]
  cat("Simulating for T =", T, "\n")
  
  # Create vectors to hold the fitted parameters
  omega_sim_t <- alpha_sim_t <- beta_sim_t <- nu_sim_t <- vector("numeric", length = S)
  
  # Simulation 
  simulations <- ugarchsim(Results$tGARCH11, m.sim = S, n.sim = T)
  
  # Fit a tGARCH for every simulation and store the parameter values
  for (j in 1:S) {
    cat("Fitting simulation:", j, "\n")
    
    # Use tryCatch to handle errors
    tryCatch({
      # Fit the GARCH model
      temp <- ugarchfit(spec = spec.2, data = simulations@simulation$seriesSim[, j])
      
      # Add parameters to vectors created
      omega_sim_t[j] <- unname(coef(temp)['omega'])
      alpha_sim_t[j] <- unname(coef(temp)['alpha1'])
      beta_sim_t[j] <- unname(coef(temp)['beta1'])
      nu_sim_t[j] <- unname(coef(temp)['shape'])
    }, error = function(e) {
      cat("Error in simulation:", j, "- Skipping\n")
      omega_sim_t[j] <- NA
      alpha_sim_t[j] <- NA
      beta_sim_t[j] <- NA
      nu_sim_t[j] <- NA
    })
  }
  
  # Update the vectors with parameter values
  omega_values_t[[i]] <- omega_sim_t
  alpha_values_t[[i]] <- alpha_sim_t
  beta_values_t[[i]] <- beta_sim_t
  nu_values_t[[i]] <- nu_sim_t
}

alpha_values_t <- Background.3_results[["alpha_values_t"]]
omega_values_t <- Background.3_results[["omega_values_t"]]
beta_values_t <- Background.3_results[["beta_values_t"]]
nu_values_t <- Background.3_results[["nu_values_t"]]


# GARCH parameters distribution ----- ----

# Create a data frame for ggplot
param_df_t <- data.frame(omega_t = omega_values_t[[4]],
                       alpha_t = alpha_values_t[[4]],
                       beta_t = beta_values_t[[4]],
                       nu_t = nu_values_t[[4]]
)

# OMEGA

ggplot(param_df_t, aes(x = omega_t)) +
  geom_density(fill = "skyblue", alpha = 0.5, linewidth = 0) +
  geom_vline(xintercept = theta_t[1], color = "blue", linetype = "dashed", linewidth = 0.3) +
  annotate("text", x = theta_t[1], y = 0, label = paste(round(theta_t[1], 8)), vjust = 1.2, hjust= 1.1, color = "blue", size = 3) +
  labs(x = "Omega Value", y = "Density") +
  theme_minimal()



# ALPHA

ggplot(param_df_t, aes(x = alpha_t)) +
  geom_density(fill = "coral", alpha = 0.5, linewidth = 0) +
  geom_vline(xintercept = theta_t[2], color = "blue", linetype = "dashed", size = 0.5) +
  annotate("text", x = theta_t[2], y = 0, label = paste(round(theta_t[2], 3)), vjust = 1.2, hjust = -0.1, color = "blue", size = 3) +
  labs(x = "Alpha Value", y = "Density") +
  theme_minimal()

# BETA

ggplot(param_df_t, aes(x = beta_t)) +
  geom_density(fill = "olivedrab3", alpha = 0.5, linewidth = 0) +
  geom_vline(xintercept = theta_t[3], color = "blue", linetype = "dashed", size = 0.5) +
  annotate("text", x = theta_t[3], y = 0, label = paste(round(theta_t[3], 3)), vjust = 1.2, hjust = -0.1, color = "blue", size = 3) +
  labs(x = "Beta Value", y = "Density") +
  theme_minimal()

#NU

ggplot(param_df_t, aes(x = nu_t)) +
  geom_density(fill = "slateblue", alpha = 0.5, linewidth = 0) +
  geom_vline(xintercept = theta_t[4], color = "blue", linetype = "dashed", size = 0.5) +
  annotate("text", x = theta_t[4], y = 0, label = paste(round(theta_t[4], 3)), vjust = 1.2, hjust = -0.1, color = "blue", size = 3) +
  labs(x = "Nu Value", y = "Density") +
  theme_minimal()



# Scatter Plot - Parameters ----

# OMEGA----
omega_df_t <- data.frame(A_250 = omega_values_t[[1]],
                       B_1000 = omega_values_t[[2]],
                       C_2500 = omega_values_t[[3]],
                       E_5000 = omega_values_t[[4]]
)

omega_df_long_t <- gather(omega_df_t, key = "Sample_Size", value = "Omega", -c())

# Specific point for theta[1, 1]
theta_point_t <- data.frame(x = 3, y = theta_t[1, 1], label = "Theta[1, 1]")

ggplot(omega_df_long_t, aes(x = Sample_Size, y = Omega)) +
  geom_point() +
  geom_hline(yintercept = theta_t[1, 1], linetype = "dashed", color = "red", linewidth = 0.4) + # Add red line
  labs(title = "Scatter Plot - Omega Values",
       x = "Sample Size",
       y = "Omega Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels= c("250", "1000", "2500", "5000"))+
  scale_y_continuous(labels = c("0.00 ", "1.0e-03", "2.0e-03", "3.0e-03", "4.0e-03" ))



# Create a scatter plot for specific sample size
ggplot(omega_df_t, aes(x = seq_along(E_5000), y = E_5000)) +
  geom_point() +
  geom_point(data = theta_point_t, aes(x = x, y = y), color = "red", size = 3) +
  geom_text(data = theta_point_t, aes(x = x, y = y, label = label), vjust = 1, hjust = 1, size = 3, color = "red") +
  labs(title = "Scatter Plot - Omega_5000",
       x = "Observation Index",
       y = "Omega Values") +
  theme_minimal()


# ALPHA ----

alpha_df_t <- data.frame(A_250 = alpha_values_t[[1]],
                         B_1000 = alpha_values_t[[2]],
                         C_2500 = alpha_values_t[[3]],
                         E_5000 = alpha_values_t[[4]]
)

alpha_df_long_t <- gather(alpha_df_t, key = "Sample_Size", value = "Alpha", -c())

# Specific point for theta[1, 1]
theta_point_t <- data.frame(x = 3, y = theta_t[2, 1], label = "Theta[2, 1]")

ggplot(alpha_df_long_t, aes(x = Sample_Size, y = Alpha)) +
  geom_point() +
  geom_hline(yintercept = theta_t[2, 1], linetype = "dashed", color = "red", linewidth = 0.4) + # Add red line
  labs(title = "Scatter Plot - Alpha Values",
       x = "Sample Size",
       y = "Alpha Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels= c("250", "1000", "2500", "5000"))



# Create a scatter plot for specific sample size
ggplot(alpha_df_t, aes(x = seq_along(E_5000), y = E_5000)) +
  geom_point() +
  geom_point(data = theta_point_t, aes(x = x, y = y), color = "red", size = 3) +
  geom_text(data = theta_point_t, aes(x = x, y = y, label = label), vjust = 1, hjust = 1, size = 3, color = "red") +
  labs(title = "Scatter Plot - Alpha_5000",
       x = "Observation Index",
       y = "Alpha Values") +
  theme_minimal()

# BETA ----

beta_df_t <- data.frame(A_250 = beta_values_t[[1]],
                         B_1000 = beta_values_t[[2]],
                         C_2500 = beta_values_t[[3]],
                         E_5000 = beta_values_t[[4]]
)

beta_df_long_t <- gather(beta_df_t, key = "Sample_Size", value = "Beta", -c())

# Specific point for theta[1, 1]
theta_point_t <- data.frame(x = 3, y = theta_t[3, 1], label = "Theta[3, 1]")

ggplot(beta_df_long_t, aes(x = Sample_Size, y = Beta)) +
  geom_point() +
  geom_hline(yintercept = theta_t[3, 1], linetype = "dashed", color = "red", linewidth = 0.4) + # Add red line
  labs(title = "Scatter Plot - Beta Values",
       x = "Sample Size",
       y = "Beta Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels= c("250", "1000", "2500", "5000"))



# Create a scatter plot for specific sample size
ggplot(beta_df_t, aes(x = seq_along(E_5000), y = E_5000)) +
  geom_point() +
  geom_point(data = theta_point_t, aes(x = x, y = y), color = "red", size = 3) +
  geom_text(data = theta_point_t, aes(x = x, y = y, label = label), vjust = 1, hjust = 1, size = 3, color = "red") +
  labs(title = "Scatter Plot - Beta_5000",
       x = "Observation Index",
       y = "Beta Values") +
  theme_minimal()



# NU ----

nu_df_t <- data.frame(A_250 = nu_values_t[[1]],
                         B_1000 = nu_values_t[[2]],
                         C_2500 = nu_values_t[[3]],
                         E_5000 = nu_values_t[[4]]
)

nu_df_long_t <- gather(nu_df_t, key = "Sample_Size", value = "Nu", -c())

# Specific point for theta[1, 1]
theta_point_t <- data.frame(x = 3, y = theta_t[4, 1], label = "Theta[4, 1]")

ggplot(nu_df_long_t, aes(x = Sample_Size, y = Nu)) +
  geom_point() +
  geom_hline(yintercept = theta_t[4, 1], linetype = "dashed", color = "red", linewidth = 0.4) + # Add red line
  labs(title = "Scatter Plot - Nu Values",
       x = "Sample Size",
       y = "Nu Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Create a scatter plot for specific sample size
ggplot(nu_df_t, aes(x = seq_along(E_5000), y = E_5000)) +
  geom_point() +
  geom_point(data = theta_point_t, aes(x = x, y = y), color = "red", size = 3) +
  geom_text(data = theta_point_t, aes(x = x, y = y, label = label), vjust = 1, hjust = 1, size = 3, color = "red") +
  labs(title = "Scatter Plot - Nu_5000",
       x = "Observation Index",
       y = "Nu Values") +
  theme_minimal()





# VAR -----

var_t_list <- list()

# Loop over the sample sizes
for (i in seq_along(T_values)) {
  T <- T_values[i]
  cat("Calculating VaR for T =", T, "\n")
  
  # Extract the simulated parameters for the current sample size
  omega_sim_t <- omega_values_t[[i]]
  alpha_sim_t <- alpha_values_t[[i]]
  beta_sim_t <- beta_values_t[[i]]
  
  # Create a data frame for the current sample size
  df_t <- data.frame(Sample_Size = as.factor(T))
  
  # Create a list to store VaR values for the current sample size
  var_t_values <- list()
  
  # Loop over the simulations
  for (j in 1:S) {
    tryCatch({
      # Calculate VaR for each simulation
      s2_t_sim <- omega_sim_t[j] + alpha_sim_t[j] * tail(y, 1)^2 + beta_sim_t[j] * tail(Results$tGARCH11@fit$var, 1)
      VaR_t_sim <- -qt(p, df= 4) * sqrt(s2_t_sim)*portfolio_value
      
      # Add VaR value to the list
      var_t_values[[j]] <- VaR_t_sim
    }, error = function(e) {
      cat("Error in simulation:", j, "- Skipping\n")
    })
  }
  
  # Add the list of VaR values to the var_list
  var_t_list[[i]] <- var_t_values
}

# Combine the list of VaR values into a single data frame
final_df_t <- data.frame(do.call(cbind, var_t_list))

# class(final_df)

colnames(final_df_t) <- c("A_250", "B_1000", "C_2500", "E_5000")

final_df_t$A_250 <- unlist(final_df_t$A_250)
final_df_t$B_1000 <- unlist(final_df_t$B_1000)
final_df_t$C_2500 <- unlist(final_df_t$C_2500)
final_df_t$E_5000 <- unlist(final_df_t$E_5000)

# Convert the data frame to long format for ggplot
final_df_t_long <- gather(final_df_t, key = "Sample_Size", value = "VaR")

final_df_t_long$VaR <- unlist(final_df_t_long$VaR)

# str(final_df_t_long$VaR)

s2_t_true <- theta_t[1,1] + theta_t[2,1] * tail(y, 1)^2 + theta_t[3,1] * tail(Results$tGARCH11@fit$var, 1)
VaR_t_true <- -qt(p, df= 4) * sqrt(s2_t_sim)*portfolio_value

# Create a scatter plot for each sample size
ggplot(final_df_t_long, aes(x = Sample_Size, y = VaR)) +
  geom_point() +
  geom_hline(yintercept = VaR_t_true, linetype = "dashed", color = "red", linewidth = 0.4) + # Add red line
  labs(title = "Scatter Plot - VaR for Different Sample Sizes",
       x = "Simulation Index",
       y = "VaR Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels= c("250", "1000", "2500", "5000"))

#Scatterplot for single Sample Size

VaR_t_point <- data.frame(x = 3, y = VaR_t_true, label = "VaR True")


ggplot(final_df_t, aes(x = seq_along(E_5000), y = E_5000)) +
  geom_point() +
  geom_point(data = VaR_t_point, aes(x = x, y = y), color = "red", size = 3) +
  geom_text(data = VaR_t_point, aes(x = x, y = y, label = label), vjust = 1, hjust = 1, size = 3, color = "red") +
  labs(title = "Scatter Plot - VaR_250",
       x = "Observation Index",
       y = "VaR Values") +
  theme_minimal()



# Learning Curves GARCH ----
T_values <- c(250, 1000, 2500, 5000)

# Initialize vectors to store MSE values for each parameter
mse_t_omega <- mse_t_alpha <- mse_t_beta <- mse_nu <- mse_t_var<- vector("numeric", length = length(T_values))

# Set the true parameter values
true_omega_t <- theta_t[1,1]  
true_alpha_t <- theta_t[2,1]  
true_beta_t <- theta_t[3,1]  
true_nu <- theta_t[4,1]  
true_VaR_t <- VaR_t_true

# Run analyses for different sample sizes
for (i in seq_along(T_values)) {
  T <- T_values[i]
  cat("Analyzing for T =", T, "\n")
  
  # Calculate MSE for each parameter
  mse_t_omega[i] <- mean((omega_values_t[[i]] - true_omega_t)^2, na.rm = TRUE)
  mse_t_alpha[i] <- mean((alpha_values_t[[i]]- true_alpha_t)^2, na.rm = TRUE)
  mse_t_beta[i] <- mean((beta_values_t[[i]] - true_beta_t)^2, na.rm = TRUE)
  mse_nu[i] <- mean((nu_values_t[[i]] - true_nu)^2, na.rm = TRUE)
  mse_t_var[i] = mean((final_df_t[[i]] - true_VaR_t)^2, na.rm = TRUE)
}

# Plot learning curves

plot(T_values, mse_t_omega, type = 'o', col = 'blue',
     xlab = 'Sample Size (T)', ylab = 'MSE(omega)',
     main = 'Learning Curve for omega')

plot(T_values, mse_t_alpha, type = 'o', col = 'green',
     xlab = 'Sample Size (T)', ylab = 'MSE(alpha)',
     main = 'Learning Curve for alpha')

plot(T_values, mse_t_beta, type = 'o', col = 'red',
     xlab = 'Sample Size (T)', ylab = 'MSE(beta)',
     main = 'Learning Curve for beta')

plot(T_values, mse_nu, type = 'o', col = 'violet',
     xlab = 'Sample Size (T)', ylab = 'MSE(nu)',
     main = 'Learning Curve for beta')

plot(T_values, mse_t_var, type = 'o', col = 'orange',
     xlab = 'Sample Size (T)', ylab = 'MSE(VaR)',
     main = 'Learning Curve for VaR')



#-----
# tGARCH simulation - GARCH fit ----
S = 1000
# Create vectors to hold the fitted parameters for different sample sizes
omega_values_tx <- alpha_values_tx <- beta_values_tx <- nu_values_tx <- vector("list", length = length(T_values))

# Run simulations for different sample sizes
for (i in seq_along(T_values)) {
  T <- T_values[i]
  cat("Simulating for T =", T, "\n")
  
  # Create vectors to hold the fitted parameters
  omega_sim_tx <- alpha_sim_tx <- beta_sim_tx <- nu_sim_tx <- vector("numeric", length = S)
  
  # Simulation 
  simulationsx <- ugarchsim(Results$tGARCH11, m.sim = S, n.sim = T)
  
  # Fit a tGARCH for every simulation and store the parameter values
  for (j in 1:S) {
    cat("Fitting simulation:", j, "\n")
    
    # Use tryCatch to handle errors
    tryCatch({
      # Fit the GARCH model with GARCH11 spec
      temp <- ugarchfit(spec = spec.1, data = simulationsx@simulation$seriesSim[, j])
      
      # Add parameters to vectors created
      omega_sim_tx[j] <- unname(coef(temp)['omega'])
      alpha_sim_tx[j] <- unname(coef(temp)['alpha1'])
      beta_sim_tx[j] <- unname(coef(temp)['beta1'])
      nu_sim_tx[j] <- unname(coef(temp)['shape'])
    }, error = function(e) {
      cat("Error in simulation:", j, "- Skipping\n")
      omega_sim_tx[j] <- NA
      alpha_sim_tx[j] <- NA
      beta_sim_tx[j] <- NA
      nu_sim_tx[j] <- NA
    })
  }
  
  # Update the vectors with parameter values
  omega_values_tx[[i]] <- omega_sim_tx
  alpha_values_tx[[i]] <- alpha_sim_tx
  beta_values_tx[[i]] <- beta_sim_tx
  nu_values_tx[[i]] <- nu_sim_tx
}
# VaR -----

library(dplyr)

# Create an empty list to store VaR values
var_list_tx <- list()

# Loop over the sample sizes
for (i in seq_along(T_values)) {
  T <- T_values[i]
  cat("Calculating VaR for T =", T, "\n")
  
  # Extract the simulated parameters for the current sample size
  omega_sim_tx <- omega_values_tx[[i]]
  alpha_sim_tx <- alpha_values_tx[[i]]
  beta_sim_tx <- beta_values_tx[[i]]
  
  # Create a data frame for the current sample size
  df <- data.frame(Sample_Size = as.factor(T))
  
  # Create a list to store VaR values for the current sample size
  var_values_tx <- list()
  
  # Loop over the simulations
  for (j in 1:S) {
    tryCatch({
      # Calculate VaR for each simulation
      s2_sim <- omega_sim_tx[j] + alpha_sim_tx[j] * tail(y, 1)^2 + beta_sim_tx[j] * tail(Results$GARCH11@fit$var, 1)
      VaR_sim_tx <- -qnorm(p, sd = sqrt(s2_sim))*portfolio_value
      
      # Add VaR value to the list
      var_values_tx[[j]] <- VaR_sim_tx
    }, error = function(e) {
      cat("Error in simulation:", j, "- Skipping\n")
    })
  }
  
  # Add the list of VaR values to the var_list
  var_list_tx[[i]] <- var_values_tx
}

# Combine the list of VaR values into a single data frame
final_df_tx <- data.frame(do.call(cbind, var_list_tx))

# class(final_df)

colnames(final_df_tx) <- c("A_250", "B_1000", "C_2500", "E_5000")

final_df_tx$A_250 <- unlist(final_df_tx$A_250)
final_df_tx$B_1000 <- unlist(final_df_tx$B_1000)
final_df_tx$C_2500 <- unlist(final_df_tx$C_2500)
final_df_tx$E_5000 <- unlist(final_df_tx$E_5000)

# Convert the data frame to long format for ggplot
final_df_long_tx <- gather(final_df_tx, key = "Sample_Size", value = "VaR")

final_df_long_tx$VaR <- unlist(final_df_long_tx$VaR)

# str(final_df_long$VaR)

s2_true <- theta[1,1] + theta[2,1] * tail(y, 1)^2 + theta[3,1] * tail(Results$GARCH11@fit$var, 1)
VaR_true <- -qnorm(p, sd = sqrt(s2_sim))*portfolio_value

# Create a scatter plot for each sample size
ggplot(final_df_long_tx, aes(x = Sample_Size, y = VaR)) +
  geom_point() +
  geom_hline(yintercept = VaR_true, linetype = "dashed", color = "red", linewidth = 0.4) + # Add red line
  labs(title = "Scatter Plot - VaR for Different Sample Sizes",
       x = "Simulation Index",
       y = "VaR Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels= c("250", "1000", "2500", "5000"))

#Scatterplot for single Sample Size

VaR_point <- data.frame(x = 3, y = VaR_true, label = "VaR True")


ggplot(final_df_tx, aes(x = seq_along(E_5000), y = E_5000)) +
  geom_point() +
  geom_point(data = VaR_point, aes(x = x, y = y), color = "red", size = 3) +
  geom_text(data = VaR_point, aes(x = x, y = y, label = label), vjust = 1, hjust = 1, size = 3, color = "red") +
  labs(title = "Scatter Plot - VaR_5000",
       x = "Observation Index",
       y = "VaR Values") +
  theme_minimal()





# Learning Curves GARCH - TGARCH ----
T_values <- c(250, 1000, 2500, 5000)

# Initialize vectors to store MSE values for each parameter
mse_omega_x <- mse_alpha_x <- mse_beta_x <- mse_var_x <- vector("numeric", length = length(T_values))

# Set the true parameter values
true_omega <- theta[1,1]  
true_alpha <- theta[2,1]  
true_beta <- theta[3,1]  
true_VaR <- VaR_true

# Run analyses for different sample sizes
for (i in seq_along(T_values)) {
  T <- T_values[i]
  cat("Analyzing for T =", T, "\n")
  
  # Calculate MSE for each parameter
  mse_omega_x[i] <- mean((omega_values_tx[[i]] - true_omega)^2, na.rm = TRUE)
  mse_alpha_x[i] <- mean((alpha_values_tx[[i]]- true_alpha)^2, na.rm = TRUE)
  mse_beta_x[i] <- mean((beta_values_tx[[i]] - true_beta)^2, na.rm = TRUE)
  mse_var_x[i] = mean((final_df_tx[[i]] - true_VaR)^2, na.rm = TRUE)
}

# Plot learning curves

plot(T_values, mse_omega_x, type = 'o', col = 'blue',
     xlab = 'Sample Size (T)', ylab = 'MSE(omega)',
     main = 'Learning Curve for omega')

plot(T_values, mse_alpha_x, type = 'o', col = 'green',
     xlab = 'Sample Size (T)', ylab = 'MSE(alpha)',
     main = 'Learning Curve for alpha')

plot(T_values, mse_beta_x, type = 'o', col = 'red',
     xlab = 'Sample Size (T)', ylab = 'MSE(beta)',
     main = 'Learning Curve for beta')

plot(T_values, mse_var_x, type = 'o', col = 'orange',
     xlab = 'Sample Size (T)', ylab = 'MSE(VaR)',
     main = 'Learning Curve for VaR')


