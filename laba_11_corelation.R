# Load necessary libraries
library(moments)
library(ggplot2)
library(corrplot)

data <- read.csv ("missile_attacks_daily.csv", header = TRUE, sep = ",")

# Convert time_start to Date type
data$time_start <- as.Date(data$time_start)

# Filter data for the year 2024
data_2024 <- subset(data, format(time_start, "%Y") == "2024" & !is.na(launched))

# Create a new column for the week number
data_2024$week <- format(data_2024$time_start, "%Y-%U")

data_2024$launched <- as.numeric(data_2024$launched)
data_2024$destroyed <- as.numeric(data_2024$destroyed)

# Create the scatter plot
correlation_plot <- ggplot(data_2024, aes(x = launched, y = destroyed)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Correlation between Missiles Launched and Taken Down in 2024",
       x = "Missiles Launched",
       y = "Missiles Taken Down") +
  theme_minimal()

# Display the plot
print(correlation_plot)

# Save the plot to a file
ggsave("correlation_launched_destroyed_2024.png", plot = correlation_plot)

# Calculate the correlation coefficient
correlation_coefficient <- cor(data_2024$launched, data_2024$destroyed, use = "complete.obs")

# Print the correlation coefficient
cat("Correlation Coefficient between Missiles Launched and Taken Down in 2024:", correlation_coefficient, "\n")

# Calculate the correlation matrix
correlation_matrix <- cor(data_2024[, c("launched", "destroyed")], use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Define the maximum lag
max_lag <- 8

# Calculate autocorrelation coefficients for launched
lag_coef_launched <- c()
for (i in 1:max_lag) {
  lag_coef_launched <- append(lag_coef_launched, cor(data_2024$launched[1:(nrow(data_2024) - i)], data_2024$launched[(i + 1):nrow(data_2024)]))
}

# Calculate autocorrelation coefficients for destroyed
lag_coef_destroyed <- c()
for (i in 1:max_lag) {
  lag_coef_destroyed <- append(lag_coef_destroyed, cor(data_2024$destroyed[1:(nrow(data_2024) - i)], data_2024$destroyed[(i + 1):nrow(data_2024)]))
}

# Create data frames for plotting
autocorr_launched <- data.frame(
  Lag = 1:max_lag,
  Coef = lag_coef_launched,
  Type = "Missiles Launched"
)

autocorr_destroyed <- data.frame(
  Lag = 1:max_lag,
  Coef = lag_coef_destroyed,
  Type = "Missiles Destroyed"
)

# Combine the data frames
autocorr <- rbind(autocorr_launched, autocorr_destroyed)

# Plot the autocorrelation function with connected dots
acf_plot <- ggplot(autocorr, aes(x = Lag, y = Coef, color = Type)) +
  geom_point() +
  geom_line() +
  labs(title = "Autocorrelation of Missiles Launched and Destroyed in 2024",
       x = "Lag",
       y = "Autocorrelation Coefficient") +
  theme_minimal() +
  scale_color_manual(values = c("Missiles Launched" = "blue", "Missiles Destroyed" = "red"))

# Display the plot
print(acf_plot)

# Save the plot to a file
ggsave("acf_launched_destroyed_2024.png", plot = acf_plot)

# Plot the histogram of the autocorrelation function
hist_plot <- ggplot(autocorr, aes(x = Lag, y = Coef, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Histogram of Autocorrelation Function",
       x = "Lag",
       y = "Autocorrelation Coefficient") +
  theme_minimal() +
  scale_fill_manual(values = c("Missiles Launched" = "blue", "Missiles Destroyed" = "red"))

# Display the histogram
print(hist_plot)

# Save the histogram to a file
ggsave("histogram_acf_launched_destroyed_2024.png", plot = hist_plot)



