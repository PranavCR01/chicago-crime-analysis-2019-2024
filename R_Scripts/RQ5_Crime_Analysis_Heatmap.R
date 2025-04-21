
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Load the dataset
crime_data <- read.csv("C:/Users/crpra/Documents/Crime_Arrests_2019-24.csv")

# Step 2: Data Cleaning - Filter relevant data and handle missing values
crime_clean <- crime_data %>%
  filter(!is.na(Location.Description) & !is.na(Primary.Type)) %>%
  mutate(Location.Category = case_when(
    grepl("STREET|ROAD|AVENUE", Location.Description, ignore.case = TRUE) ~ "Street",
    grepl("RESIDENCE|HOME|APARTMENT", Location.Description, ignore.case = TRUE) ~ "Residential",
    grepl("PARK|SCHOOL|PUBLIC", Location.Description, ignore.case = TRUE) ~ "Public Space",
    TRUE ~ "Other"
  ))

# Step 3: Summarize data - Count crimes by Location Category and Crime Type
crime_summary <- crime_clean %>%
  group_by(Location.Category, Primary.Type) %>%
  summarise(Crime_Count = n(), .groups = "drop")

# Step 4: Prepare data for heatmap
crime_heatmap <- crime_summary %>%
  pivot_wider(names_from = Primary.Type, values_from = Crime_Count, values_fill = 0) %>%
  pivot_longer(cols = -Location.Category, names_to = "Crime.Type", values_to = "Crime_Count")

# Step 5: Generate Heatmap Visualization
heatmap_plot <- ggplot(crime_heatmap, aes(x = Crime.Type, y = Location.Category, fill = Crime_Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Crime_Count), color = "black", size = 3) +  # Annotate crime counts
  scale_fill_gradient(low = "lightblue", high = "darkred") +       # Adjust color scale
  labs(
    title = "Crime Distribution Across Location Categories",
    x = "Crime Type",
    y = "Location Category",
    fill = "Crime Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate X-axis labels for clarity

# Step 6: Save Heatmap to File
ggsave("Crime_Location_Heatmap.png", plot = heatmap_plot, width = 10, height = 6, dpi = 300)

# Step 7: Export Processed Data to CSV
write.csv(crime_heatmap, "Crime_Location_Heatmap_Data.csv", row.names = FALSE)

# Print Confirmation
print("Heatmap successfully generated and saved as 'Crime_Location_Heatmap.png'")
print("Processed data saved as 'Crime_Location_Heatmap_Data.csv'")
