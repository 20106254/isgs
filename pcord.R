library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

source("custom_theme.R")

data <- read.csv("RELEVE_SP_DATA.txt")

data_clean <- data %>%
  select(-ID, -RELEVE_ID) 

data_aggregated <- data_clean %>%
  group_by(SITE_ID, SPECIES_NAME) %>%
  summarise(DOMIN = sum(DOMIN), .groups = "drop")

data_wide <- data_aggregated %>%
  spread(key = SPECIES_NAME, value = DOMIN, fill = 0)

data_numeric <- data_wide %>%
  select(-SITE_ID) 

if (any(data_numeric < 0)) {
  warning("The dataset contains negative values, which will be adjusted.")
  data_numeric[data_numeric < 0] <- 0  
}

data_scaled <- scale(data_numeric)  
nmds_result <- metaMDS(data_scaled, distance = "horn", k = 2, trymax = 20)
nmds_points <- data.frame(nmds_result$points)
nmds_points$Site <- data_wide$SITE_ID  

p <- ggplot(nmds_points, aes(x = MDS1, y = MDS2, label = Site)) +
  geom_point(size = 0.25, color = "blue") +
  ggtitle("NMDS Ordination of Species Data") +
  custom_theme +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

ggsave("nmds_plot.svg", plot = p, width = 10, height = 10)

print("NMDS plot saved as: nmds_plot.svg")

