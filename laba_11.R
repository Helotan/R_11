# Load necessary libraries
library(ggplot2)

data <- read.csv ("missile_attacks_daily.csv", header = TRUE, sep = ",")

# Convert time_start to Date type
data$time_start <- as.Date(data$time_start)

# Filter data for the year 2024
data_2024 <- subset(data, format(time_start, "%Y") == "2024" & !is.na(launched))

# Create a new column for the week number
data_2024$week <- format(data_2024$time_start, "%Y-%U")

# Plot time_start vs launched
plot(data_2024$time_start, data_2024$launched, 
     xlab = "Час атаки", 
     ylab = "Кількість запущених ракет", 
     main = "Ракетні удари протягом 2024 року",
     pch = 19, 
     col = "blue")

# Count attacks per target
target_counts <- table(data_2024$target)
par(mar = c(18, 0, 0, 0))
# Sort target counts in descending order
target_counts <- sort(target_counts, decreasing = TRUE)
# Plot histogram
bar_positions <- barplot(target_counts, 
       main = "Атаки по областях за 2024 рік", 
       cex.names = 1,
       ylab = "Кількість ракетних ударів", 
       col = "lightblue", 
       las = 2)

mtext("Область/регіон атаки", side = 1, line = -4)
mtext("Кількість ракетних ударів", side = 2, line = -1)
text(x = bar_positions, y = target_counts, label = target_counts, pos = 3, cex = 0.8, col = "black")

mean_launched <- mean(data_2024$launched)
mode_launched <- as.numeric(names(sort(table(data_2024$launched), decreasing = TRUE)[1]))
median_launched <- median(data_2024$launched)
range_launched <- range(data_2024$launched)
sd_launched <- sd(data_2024$launched)
cv_launched <- sd_launched / mean_launched * 100
skewness_launched <- skewness(data_2024$launched)
kurtosis_launched <- kurtosis(data_2024$launched)
min_launched <- min(data_2024$launched)
max_launched <- max(data_2024$launched)
sum_launched <- sum(data_2024$launched)


# Print statistics
cat("Середнє арифметичне (Mean):", mean_launched, "\n")
cat("Мода (Mode):", mode_launched, "\n")
cat("Медіана (Median):", median_launched, "\n")
cat("Розмах (Range):", range_launched, "\n")
cat("Стандартне відхилення (Standard Deviation):", sd_launched, "\n")
cat("Коефіцієнт варіації (Coefficient of Variation):", cv_launched, "%\n")
cat("Асиметрія (Skewness):", skewness_launched, "\n")
cat("Ексцес (Kurtosis):", kurtosis_launched, "\n")
cat("Мінімум (Minimum):", min_launched, "\n")
cat("Максимум (Maximum):", max_launched, "\n")
cat("Сума (Sum):", sum_launched, "\n")

# Count the number of attacks per week
weekly_attacks <- data_2024 %>%
  group_by(week) %>%
  summarise(attacks = n())

# Calculate cumulative attacks
weekly_attacks$cumulative_attacks <- cumsum(weekly_attacks$attacks)

# Create the ggplot
p <- ggplot(weekly_attacks, aes(x = as.Date(paste0(week, "-1"), format = "%Y-%U-%u"), y = cumulative_attacks)) +
  geom_line(color = "blue") +
  labs(title = "Кумулянта атак у 2024",
       x = "Тиждень",
       y = "Кумулятивна кількість атак") +
  theme_minimal()

# Save the plot to a file
ggsave("cumulative_missile_attacks_2024.png", plot = p)

# Create the ggplot for histogram of weekly attacks
histogram_plot <- ggplot(weekly_attacks, aes(x = as.Date(paste0(week, "-1"), format = "%Y-%U-%u"), y = attacks)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Частота атак по тижнях у 2024 році",
       x = "Тиждень",
       y = "Кількість атак") +
  theme_minimal()

# Save the histogram plot to a file
ggsave("weekly_frequency_attacks_2024.png", plot = histogram_plot)

# Display the histogram plot
print(histogram_plot)

# Create the ggplot for cumulative attacks with dots and horizontal lines
cumulative_plot <- ggplot(weekly_attacks, aes(x = as.Date(paste0(week, "-1"), format = "%Y-%U-%u"), y = cumulative_attacks)) +
  geom_point(color = "blue") +
  geom_segment(aes(x = as.Date(paste0(week, "-1"), format = "%Y-%U-%u"), xend = lead(as.Date(paste0(week, "-1"), format = "%Y-%U-%u")), yend = cumulative_attacks), color = "blue") +
  labs(title = "Кумулянта атак у 2024",
       x = "Тиждень",
       y = "Кумулятивна кількість атак") +
  theme_minimal()

# Save the cumulative plot to a file
ggsave("cumulative_attacks_based_on_histogram_2024.png", plot = cumulative_plot)

# Display the cumulative plot
print(cumulative_plot)





