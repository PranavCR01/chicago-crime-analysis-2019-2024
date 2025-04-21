library(lubridate)
library(ggplot2)
library(dplyr)


file_path <- "C:/Users/tejas/Downloads/Crime_Arrests_2019-24.csv"
crime_data <- read.csv(file_path)

crime_data$date_time <- as.POSIXct(crime_data$date, format = "%Y-%m-%d %H:%M:%S")
crime_data$date <- as.Date(crime_data$date, format="%Y-%m-%d")

# Extract months, and seasons
crime_data$month <- month(crime_data$date, label = TRUE)
crime_data$season <- case_when(
  month(crime_data$date) %in% c(12, 1, 2) ~ "Winter",
  month(crime_data$date) %in% c(3, 4, 5) ~ "Spring",
  month(crime_data$date) %in% c(6, 7, 8) ~ "Summer",
  month(crime_data$date) %in% c(9, 10, 11) ~ "Fall"
)

#Plot for Crimes by Month
ggplot(crime_data, aes(x = month)) +
  geom_bar(fill = "orange") +
  theme_minimal() +
  labs(title = "Monthly Crime Rates", x = "Month", y = "Count")

#Plot for Crimes by Season
ggplot(crime_data, aes(x = season)) +
  geom_bar(fill = "green") +
  theme_minimal() +
  labs(title = "Seasonal Crime Rates", x = "Season", y = "Count")


#-------------------------------------------------------------------

monthly_crime_summary <- crime_data %>%
  group_by(month, crime_type = crime_data$primary.type) %>%
  summarise(count = n(), .groups = "drop")


# Get the top 5 crimes per month
top_crimes <- monthly_crime_summary %>%
  group_by(month) %>%
  top_n(5, count) %>%
  arrange(month, desc(count))

# Plot for the top 5 crimes per month
ggplot(top_crimes, aes(x = reorder(crime_type, -count), y = count, fill = crime_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~month, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Top 5 Crimes Committed Per Month",
    x = "Crime Type",
    y = "Count",
    fill = "Crime Type"
  )

# Plot for Stacked Bar Graph
ggplot(top_crimes, aes(x = month, y = count, fill = crime_type)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(
    title = "Top 5 Crimes Committed Per Month",
    x = "Month",
    y = "Count",
    fill = "Crime Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#-------------------------------------------------------------------

crime_data$hour <- hour(crime_data$date_time)

crime_data$time_of_day <- case_when(
  crime_data$hour >= 6 & crime_data$hour < 12 ~ "Morning",
  crime_data$hour >= 12 & crime_data$hour < 16 ~ "Afternoon",
  crime_data$hour >= 16 & crime_data$hour < 20 ~ "Evening",
  crime_data$hour >= 20 & crime_data$hour < 24 ~ "Night",
  crime_data$hour >= 0 & crime_data$hour < 6 ~ "Midnight"
)

# Group by time of day and count the number of crimes
time_of_day_summary <- crime_data %>%
  group_by(time_of_day) %>%
  summarise(count = n(), .groups = "drop")

# Order the time of day
time_of_day_summary$time_of_day <- factor(
  time_of_day_summary$time_of_day, 
  levels = c("Morning", "Afternoon", "Evening", "Night", "Midnight")
)

# Plot for bar Graph for Crimes by Time of Day
ggplot(time_of_day_summary, aes(x = time_of_day, y = count, fill = time_of_day)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Number of Crimes by Time of Day",
    x = "Time of Day",
    y = "Count",
    fill = "Time of Day"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#-------------------------------------------------------------------