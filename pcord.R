# Load necessary libraries
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

source("custom_theme.R")

# Step 1: Load data from CSV (replace with your actual file path)
data <- read.csv("RELEVE_SP_DATA.txt")

# Step 2: Remove unnecessary columns (ID and RELEVE_ID)
data_clean <- data %>%
  select(-ID, -RELEVE_ID)  # Removing ID and RELEVE_ID as they are not needed for the analysis

# Step 3: Aggregate the data to ensure unique combinations of SITE_ID and SPECIES_NAME
# Summing the DOMIN values for each species at each site
data_aggregated <- data_clean %>%
  group_by(SITE_ID, SPECIES_NAME) %>%
  summarise(DOMIN = sum(DOMIN), .groups = "drop")

# Step 4: Reshape the data from long format to wide format
# The species names become columns and the values are the summed DOMIN values
data_wide <- data_aggregated %>%
  spread(key = SPECIES_NAME, value = DOMIN, fill = 0)

# Step 5: Remove the SITE_ID column, leaving only species data
data_numeric <- data_wide %>%
  select(-SITE_ID)  # Keep only species columns for analysis

# Step 6: Check for negative values in the data
if (any(data_numeric < 0)) {
  warning("The dataset contains negative values, which will be adjusted.")
  data_numeric[data_numeric < 0] <- 0  # Replace negative values with zero
}

# Step 7: Standardize the data (scale it)
data_scaled <- scale(data_numeric)  # Standardize the data

# Step 8: Check for any negative values after scaling
if (any(data_scaled < 0)) {
  warning("The scaled dataset contains negative values.")
}

nmds_result <- metaMDS(data_scaled, distance = "horn", k = 2, trymax = 20)
#dissimilarity <- vegdist(data_scaled, method = "jaccard")  # For presence/absence data
# If using abundance data and you want the Sorensen Index, you can use 'bray' as it's also suitable for abundance data

# Step 3: Perform NMDS using the Sorensen index for dissimilarity
#nmds_result <- metaMDS(dissimilarity, k = 2, trymax = 100)

# Step 10: Extract the NMDS points for plotting
nmds_points <- data.frame(nmds_result$points)
nmds_points$Site <- data_wide$SITE_ID  # Add Site information to the plot data

# Step 11: Plot the NMDS results using ggplot2
p <- ggplot(nmds_points, aes(x = MDS1, y = MDS2, label = Site)) +
  geom_point(size = 0.25, color = "blue") +
  ggtitle("NMDS Ordination of Species Data") +
  scale_x_continuous(limits = c(-2.0, 2.0)) +  # Set x-axis limits
  scale_y_continuous(limits = c(-1.5, 1.5)) +  # Set y-axis limits
  custom_theme +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

# Step 12: Save the plot to an SVG file
ggsave("nmds_plot.svg", plot = p, width = 10, height = 10)

# Print out the path where the plot is saved
print("NMDS plot saved as: nmds_plot.svg")

