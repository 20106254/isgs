# Load necessary libraries
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

source("custom_theme.R")

# Step 1: Load data from CSV (replace with your actual file path)
data <- read.csv("RELEVE_SP_DATA.txt")

data_cleaned <- data |>
  group_by(SITE_ID, SPECIES_NAME) |>
  summarise(DOMIN = mean(DOMIN, na.rm = TRUE), .groups = 'drop')

# Reshape the data into a wide format (sites as rows, species as columns)
wide_data <- data_cleaned %>%
  tidyr::pivot_wider(names_from = SPECIES_NAME, values_from = DOMIN, values_fill = list(Domin = 0))


# Step 5: Remove the SITE_ID column, leaving only species data
data_numeric <- wide_data %>%
  select(-SITE_ID)  # Keep only species columns for analysis

data_numeric[is.na(data_numeric)] <- 0

# Step 7: Standardize the data (scale it)
data_scaled <- scale(data_numeric)  # Standardize the data

str(data_numeric)
# Calculate a dissimilarity matrix (Bray-Curtis dissimilarity is commonly used in ecology)
dissimilarity_matrix <- vegdist(data_numeric, method = "bray")

# Perform hierarchical clustering using Ward's method
hclust_result <- hclust(dissimilarity_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hclust_result, main = "Dendrogram of Site Clustering", xlab = "Sites", sub = "", ylab = "Bray-Curtis Dissimilarity")

# Cut the dendrogram into clusters (e.g., 3 clusters)
clusters <- cutree(hclust_result, k = 3)

# Add cluster assignments to the original wide data
wide_data$Cluster <- clusters

# View the final data with cluster assignments
print(wide_data)

svg("dp-wk-nmds_plot.svg", width = 10, height = 10)  # Open an SVG graphics device
heatmap(as.matrix(wide_data),
        Rowv = as.dendrogram(hclust_result),
        Colv = NA,
        scale = "row",
        col = colorRampPalette(c("white", "blue"))(100),
        main = "Heatmap of Species Dominance by Site")
dev.off()  # Close the SVG graphics device


