###############################################################################
# Code to visualize and analyse 3D data on population, habitat, and climate
# for CUs in the Fraser River basin
###############################################################################

library(dplyr)
library(plotly) # for 3D plots


# Functions to connect to PSF database
source("https://raw.githubusercontent.com/salmonwatersheds/population-indicators/refs/heads/master/code/functions_general.R")

# PSf colours and species colours
source("https://raw.githubusercontent.com/salmonwatersheds/population-indicators/refs/heads/master/code/colours.R")
# Transform all scores into a single number, then transform that number to be min = 0 and max = 1

#------------------------------------------------------------------------------
# Read in data (see data-processing.R for compilation steps)
#------------------------------------------------------------------------------

data <- read.csv("output/CU_pophabclim_scores.csv")

# Scale data for plotting and cluster analysis
scaled_data <- data %>%
  mutate(pop_score = scale(pop_score),
         hab_score = scale(hab_score),
         clim_score = scale(clim_score))

#------------------------------------------------------------------------------
# 3D plot
#------------------------------------------------------------------------------

# By species
fig <- plot_ly(scaled_data, x = ~pop_score, y = ~hab_score, z = ~clim_score, color = ~species_name, colors = species_cols_light, text = paste(data$cu_name_pse, data$species_name), hoverinfo = 'text')
fig <- fig %>% add_markers() 
fig # Display the plot

# Traditional approach based on biological status
data$recovery_approach <- as.numeric(population$score[match(data$cuid, population$cuid)] > 2.5) + 1

fig <- plot_ly(data, x = ~population, y = ~habitat, z = ~climate, color = ~recovery_approach, colors = status_cols[c('green', 'red')], text = paste(data$cu_name_pse, data$species_name), hoverinfo = 'text')
fig <- fig %>% add_markers() 
fig # Display the plot

#------------------------------------------------------------------------------
# Cluster analysis
#------------------------------------------------------------------------------
library(viridisLite)
n_clust <- 5

#-----
# K-means
#-----
# Run K-means with, for example, 3 clusters
set.seed(123) # For reproducibility
kmeans_result <- kmeans(scaled_data, centers = n_clust, nstart = 25)

# Add cluster assignments back to original data
data$k_cluster <- as.factor(kmeans_result$cluster)

# View results
print(kmeans_result$centers) # Interpretation of cluster characteristics
write.csv(kmeans_result$centers, "output/k-means_clusters.csv")

table(data$k_cluster)      # Size of each cluster

# visualize 3D with points coloured by cluster
fig <- plot_ly(data, x = ~population, y = ~habitat, z = ~climate, color = ~k_cluster, colors = viridis(n = n_clust), text = paste(data$cu_name_pse, data$species_name), hoverinfo = 'text')
fig <- fig %>% add_markers() 
fig # Display the plot

# Simple 2D Visualization using the first two principal components
library(factoextra)
fviz_cluster(list(data = scaled_data %>% select(pop_score, hab_score, clim_score), cluster = kmeans_result$cluster), palette = viridis(n = n_clust))


#-----
# Hierarchical clustering
#-----

# 1. Compute distance matrix
d <- dist(scaled_data, method = "euclidean")

# 2. Perform hierarchical clustering (Ward's method is usually best)
h_clust <- hclust(d, method = "ward.D2")

# 3. Plot the dendrogram
plot(h_clust, labels = paste(all_complete$cu_name_pse, all_complete$species_name), main = "Dendrogram")

# 4. Cut the tree into a specific number of groups (e.g., 3)
groups <- cutree(h_clust, k = n_clust)
rect.hclust(h_clust, k = n_clust, border = viridis(n = n_clust)) # Add boxes to plot


