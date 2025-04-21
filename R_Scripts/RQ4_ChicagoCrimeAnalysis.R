# Load necessary libraries 
library(dplyr) 

library(ggplot2) 

library(lubridate) 

library(tidyr) 

library(tsibble) 

# Read the dataset  

data <- read.csv("/Users/arundhatiraj/Downloads/Crime_Arrests_2019-24.csv") 

# Renaming the columns to make them more understandable 

colnames(data) <- c("id", "case_number", "date", "block", "iucr", "primary_type",  
                    
                    "description", "location_description", "arrest", "domestic",  
                    
                    "beat", "district", "ward", "community_area", "fbi_code",  
                    
                    "x_coordinate", "y_coordinate", "year", "updated_on", "latitude",  
                    
                    "longitude", "location", "cb_no", "arrest_date", "race",  
                    
                    "charge1_statute", "charge1_description", "charge1_type",  
                    
                    "charge1_class", "charge2_statute", "charge2_description",  
                    
                    "charge2_type", "charge2_class", "charge3_statute",  
                    
                    "charge3_description", "charge3_type", "charge3_class",  
                    
                    "charge4_statute", "charge4_description", "charge4_type",  
                    
                    "charge4_class", "charges_statute", "charges_description",  
                    
                    "charges type", "charges_class") 

# Convert 'arrest' to a factor (arrest status: TRUE or FALSE) 

data$arrest <- as.factor(data$arrest) # Arrest status: TRUE or FALSE 

data$arrest_date <- as.Date(data$arrest_date, format="%Y-%m-%d") 

# Filter data for years 2019-2024 

data_filtered <- data %>% 
  
  filter(year >= 2019 & year <= 2024) 

# Group the data by crime type, year, and arrest status 

crime_arrest_data <- data_filtered %>% 
  
  group_by(primary_type, year, arrest) %>% 
  
  summarise(count = n(), .groups = 'drop') 

# Create a line plot for trends in arrests vs non-arrests by crime type and year 

ggplot(crime_arrest_data, aes(x = year, y = count, color = arrest, group = interaction(primary_type, arrest))) + 
  
  geom_line() + 
  
  labs(x = "Year", y = "Number of Crimes",  
       
       title = "Trends in Arrests vs Arrests Not Made by Crime Type (2019-2024)") + 
  
  facet_wrap(~primary_type, scales = "free_y") + # Facet by crime type 
  
  scale_color_manual(values = c("blue", "red")) + # Blue for arrests, Red for non-arrests 
  
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
> 