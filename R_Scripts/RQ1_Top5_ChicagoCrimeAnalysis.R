# Install and load necessary libraries
install.packages("tidyverse")
install.packages("forecast")
install.packages("naniar")
library(tidyverse)
library(forecast)
library(naniar)

# Load the dataset
crime_data <- read.csv("C:/Users/crpra/Documents/Crime_Arrests_2019-24.csv")

# Ensure the date column is in Date format
crime_data$date <- as.Date(crime_data$date)

# Filter data for the years 2019 to 2024
crime_data <- crime_data %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2024-12-31"))

# Extract year for further analysis
crime_data <- crime_data %>%
  mutate(year = format(date, "%Y"))

# Exploratory Data Analysis (EDA)

# 1. Summary of the date range
summary(crime_data$date)

# 2. Unique values in the 'primary type' column
unique_primary_types <- unique(crime_data$`primary.type`)
print(unique_primary_types)

# 3. Top 5 Crime Types Analysis
# Aggregate data to find the top 5 crime types
crime_type_agg <- crime_data %>%
  group_by(`primary.type`) %>%
  summarise(total_count = n(), .groups = 'drop') %>%
  arrange(desc(total_count)) %>%
  slice_head(n = 5)

print(crime_type_agg)

# Filter data for the top 5 crime types
crime_data_top5 <- crime_data %>%
  filter(`primary.type` %in% crime_type_agg$`primary.type`)

# Yearly trends of top 5 crimes
crime_trends_top5 <- crime_data_top5 %>%
  group_by(`primary.type`, year) %>%
  summarise(count = n(), .groups = 'drop')

# Enhanced Yearly Trends Plot for Top 5 Crimes
ggplot(crime_trends_top5, aes(x = year, y = count, color = `primary.type`, group = `primary.type`)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Yearly Trends for Top 5 Crime Types (2019-2024)",
    subtitle = "Analysis of Crime Trends Over the Past Five Years",
    x = "Year",
    y = "Crime Count",
    color = "Types Of Crime"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(1.5, "cm"), # Increase space between legend keys
    legend.spacing.x = unit(0.5, "cm")  # Add spacing between legend items
  ) +
  guides(color = guide_legend(nrow = 2)) # Wrap legend items into two rows




