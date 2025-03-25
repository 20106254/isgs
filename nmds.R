library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(cluster) 
source("save_plot.R")
source("custom_theme.R")

data <- read.csv("output.csv")

data_clean <- data %>%
  select(-ID, -RELEVE_ID) 

data_aggregated <- data_clean %>%
  group_by(SITE_ID, SPECIES_NAME) %>%
  summarise(DOMIN = sum(DOMIN), .groups = "drop")

data_wide <- data_aggregated %>%
  spread(key = SPECIES_NAME, value = DOMIN, fill = 0)

data_numeric <- data_wide %>%
  select(-SITE_ID) 

dissimilarity_matrix <- vegdist(data_numeric, method = "bray", memb.exp = 1.1, max_iterations = 10000)
fanny_result <- fanny(dissimilarity_matrix, k = 3, memb.exp = 1.1)  
data_wide$FANNY_Cluster <- fanny_result$clustering

nmds_result <- metaMDS(data_numeric, distance = "bray", k = 3, trymax = 1000)
cat("Stress value:", nmds_result$stress, "\n")
nmds_points <- data.frame(nmds_result$points)
nmds_points$Site <- data_wide$SITE_ID  
nmds_points$FANNY_Cluster <- as.factor(data_wide$FANNY_Cluster)  

p <- ggplot(nmds_points, aes(x = MDS1, y = MDS2, color = FANNY_Cluster, label = Site)) +
  geom_point(size = 0.25) +  
  ggtitle("NMDS Ordination with FANNY Clustering") +
  custom_theme +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) 


save_plot(p, "nmds-isgs-data.svg")
