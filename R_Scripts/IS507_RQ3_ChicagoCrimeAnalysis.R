# Install necessary packages (if not already installed)
install.packages("tidyverse", type = "binary")

# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
#library(boxplot)

# Step 1: Load master crime dataset CSV
crime_data <- read_csv("/Users/michaelkhalaf/Desktop/IS507CSVs/Crime_Arrests_2019-24.csv")

# Load additional CSVs (ensure they are in your working directory)
iucr_data <- read_csv("/Users/michaelkhalaf/Desktop/IS507CSVs/unique_iucr_sorted.csv")
crime_names <- read_csv("/Users/michaelkhalaf/Desktop/IS507CSVs/unique_crime_names.csv")

# Preview the datasets
head(crime_data)
head(iucr_data)
head(crime_names)

# Step 2: Data Filter for Dates
# Convert 'date' to datetime format and filter for 2019–2024
crime_data <- crime_data %>%
  mutate(date = as_datetime(date),  # Convert date column
         year = year(date)) %>%     # Extract year
  filter(year >= 2019 & year <= 2024)  # Filter from 2019 to 2024

# Preview data filter
head(crime_data)

# Step 3: Define Crime Groups by physical and property crime (general consensus)
physical_crimes <- c("BATTERY", "ASSAULT", "SEXUAL ASSAULT", "HOMICIDE", "ROBBERY")
property_crimes <- c("THEFT", "BURGLARY", "FORGERY", "ARSON", "MOTOR VEHICLE THEFT")

# Classify crimes into physical or property categories
crime_data <- crime_data %>%
  mutate(crime_category = case_when(
    'primary type' %in% physical_crimes ~ "Physical",
    'primary type' %in% property_crimes ~ "Property",
    TRUE ~ "Other"  # For all other crimes
  ))
colnames(crime_data) 
# Step 4: Split Data by Crime Category
# Filter physical crimes (violence)
physical_crimes_data <- crime_data %>%
  filter(crime_category == "Physical")

# Filter property crimes
property_crimes_data <- crime_data %>%
  filter(crime_category == "Property")

# Step 5: Summarize Data Annually
# Summarize physical crimes by year
physical_summary <- physical_crimes_data %>%
  group_by(year) %>%
  summarise(total_physical_crimes = n())

# Summarize property crimes by year
property_summary <- property_crimes_data %>%
  group_by(year) %>%
  summarise(total_property_crimes = n())

# Combine summaries for comparison
annual_summary <- full_join(physical_summary, property_summary, by = "year") %>%
  replace_na(list(total_physical_crimes = 0, total_property_crimes = 0))

# Step 6: Save Results
# Save the filtered datasets
write_csv(physical_crimes_data, "physical_crimes.csv")
write_csv(property_crimes_data, "property_crimes.csv")
write_csv(annual_summary, "annual_crime_summary.csv")

# Step 7: Time-Series Analysis with ggplot2
# Plot annual trends for physical and property crimes
ggplot(annual_summary, aes(x = year)) +
  geom_line(aes(y = total_physical_crimes, color = "Physical Crimes"), size = 1) +
  geom_line(aes(y = total_property_crimes, color = "Property Crimes"), size = 1) +
  labs(title = "Annual Crime Trends (2019–2024)",
       x = "Year", y = "Number of Incidents", color = "Crime Type") +
  theme_minimal()

# Step 8: Boxplot Analysis
# Boxplot of incidents by crime category
ggplot(crime_data, aes(x = crime_category, y = year)) +
  geom_boxplot(aes(fill = crime_category)) +
  labs(title = "Crime Distribution by Category (2019–2024)",
       x = "Crime Category", y = "Year", fill = "Category") +
  theme_minimal()