###############################################################################
# Code to visualize and analyse 3D data on population, habitat, and climate
# for CUs in the Fraser River basin
###############################################################################

library(dplyr)
library(plotly) # for 3D plots
library(viridisLite)


# Functions to connect to PSF database
source("https://raw.githubusercontent.com/salmonwatersheds/population-indicators/refs/heads/master/code/functions_general.R")

# PSf colours and species colours
source("https://raw.githubusercontent.com/salmonwatersheds/population-indicators/refs/heads/master/code/colours.R")
names(status_cols)[1:3] <- c("good", "fair", "poor")

#------------------------------------------------------------------------------
# Read in data (see data-processing.R for compilation steps)
#------------------------------------------------------------------------------

data <- read.csv("output/CU_pophabclim_scores.csv")

# Scale data for plotting and cluster analysis
scaled_data <- data %>%
  mutate(pop_score = scale(pop_score),
         hab_score = scale(hab_score),
         clim_score = scale(clim_score))

scaled_data$species_name <- factor(scaled_data$species_name, levels = names(species_cols_dark))
scaled_data$biostatus <- factor(scaled_data$biostatus, levels = c("good", "fair", "poor"))

scaled_data <- scaled_data %>% arrange(species_name, pop_score)

#------------------------------------------------------------------------------
# 2D plot
#------------------------------------------------------------------------------

# Function to set up 2D plotting region:
plot_2Dhabbio <- function(axis.lwd = 3){
  plot(c(-2, 2), c(-2, 1.2), "n", xlim = c(-2, 2), ylim = c(-2, 1.2), xaxs = "i", yaxs = "i", xlab = "", ylab= "", xaxt = "n", yaxt = "n", bty = "n")
  
  # for(i in 1:3){
  #   polygon(x = i - 1 + c(0.5, 1.5, 1.5, 0.5), y = c(0.3, 0.3, 0.5, 0.5), col = status_cols[i], xpd = NA, border = NA)
  # }
  
  for(i in 1:100){
    segments(x0 = -2.1, x1 = -2.01, y0 = seq(-2, 1.2, length.out = 100)[i], y1 = seq(-2, 1.2, length.out = 100)[i], col = clim_cols[i], xpd = NA, lwd = axis.lwd, lend = 2)
  }
  axis(side = 2, at = c(-1.8, 0, 1), labels = c("Better", "Average", "Worse"), tck = 0)
  
  # for(j in 1:3){
  #   polygon(x = c(0.3, 0.3, 0.5, 0.5), j - 1 + c(0.5, 1.5, 1.5, 0.5), y = , col = status_cols[j], xpd = NA, border = NA)
  # }
  for(j in 1:100){
    segments(x0 = seq(-2, 2, length.out = 100)[j], x1 = seq(-2, 2, length.out = 100)[j], y0 = -2.1, y1 = -2.01, col = clim_cols[j], xpd = NA, lwd = axis.lwd, lend = 2)
  }
  axis(side = 1, at = c(-1.6, 0, 1.6), labels = c("Better", "Average", "Worse"), tck = 0)
  
  # abline(h = c(1:2) + 0.5)
  # abline(v = c(1:2) + 0.5)
  abline(h = 0)
  abline(v = 0)
  
  # polygon(x = c(-2, -2, 2, 2), y = c(-2, 1.2, 1.2, -2))
  

}

clim_cols <- colorRampPalette(status_cols[c('good', 'fair', 'poor')])(n = 101)
clim_breaks <- seq(-2, 2, length.out = 100)

# Plot
quartz(width = 8, height = 4.5, pointsize = 10)
par(mfrow = c(1,2), mar = c(4,4,5,2), oma = c(0,0,0,0))

# CUrrent recovery paradigm
plot_2Dhabbio()
points(scaled_data$hab_score, scaled_data$pop_score, pch = ifelse(scaled_data$biostatus == "poor", 19, 21), bg = "white", cex = 1.8)
mtext(side = 3, line = 4, adj = 0, "a)", cex = 1.2)
mtext(side = 1, "Habitat status", line = 2.5, cex = 1.2)
mtext(side =2, "Biological status", line =2.5, cex = 1.2)

legend(-1, 2.1, xpd = NA, pch = c(19, 21), pt.cex = 1.8, legend = c("Poor", "Fair/Good"), title ="Biological status outcome", ncol = 2, bty = "n", cex = 1.2)

# Resilience paradigm
plot_2Dhabbio()
points(scaled_data$hab_score, scaled_data$pop_score, pch = 21, bg = clim_cols[findInterval(scaled_data$clim_score, clim_breaks)], cex = 1.8)
mtext(side = 3, line = 4, adj = 0, "b)", cex = 1.2)
mtext(side = 1, "Habitat status", line = 2.5, cex = 1.2)
mtext(side =2, "Biological status", line =2.5, cex = 1.2)


# Climate legend
# x0 <- 3.2; x1 <- 3.3
# for(j in 1:100){
#   segments(x0 = x0, x1 = x1, y0 = seq(-2, 1, length.out = 100)[j], y1 = seq(-2, 1, length.out = 100)[j], col = clim_cols[j], xpd = NA, lwd = 3, lend = 2)
# }
# text(rep(3, 3), c(-2, mean(c(-2, 1)), 1), c("Low", "Average", "High"), xpd = NA, adj = 1)# polygon(x = c(x0, x0, x1, x1), y = c(-2, 1.2, 1.2, -2), xpd = NA)
# text(2.7, 1.4, "Climate change\nexposure", xpd = NA, cex = 1.2)

x <- seq(-2, 2, length.out = 100)
y0 = 1.6; y1 <- 1.7
for(j in 1:100){
  segments(x0 = x[j], x1 = x[j], y0 = y0, y1 = y1, col = clim_cols[j], xpd = NA, lwd = 3, lend = 2)
}

text(0,1.8, "Climate change exposure", xpd = NA, cex = 1.2, pos = 3)
text(c(-2, 0, 2), rep(1.5, 3), c("Low", "Average", "High"), xpd = NA)# polygon(x = c(x0, x0, x1, x1), y = c(-2, 1.2, 1.2, -2), xpd = NA)
#------------------------------------------------------------------------------
# 3D plot by species
#------------------------------------------------------------------------------

scaled_data <- scaled_data %>% arrange(species_name, pop_score)

# By species
fig <- plot_ly(
  scaled_data, # Need to arrange to avoid misrep
  x = ~pop_score, y = ~hab_score, z = ~clim_score, 
  color = ~species_name, colors = species_cols_light, 
  text = ~paste(cu_name_pse, species_name), hoverinfo = 'text') %>%
  layout(scene = list(
    xaxis = list(title = "Biological"),
    yaxis = list(title = "Habitat"),
    zaxis = list(title = "Climate change",
                 aspectmode = 'data') # Avoids re-scaling
  )) %>%
  add_markers() 
fig # Display the plot

#------------------------------------------------------------------------------
# 3D plot by biological status
#------------------------------------------------------------------------------

scaled_data <- scaled_data %>% arrange(biostatus, pop_score)

fig <- plot_ly(
  scaled_data, 
  x = ~pop_score, y = ~hab_score, z = ~clim_score, 
  color = ~biostatus, colors = status_cols, 
  text = ~paste(cu_name_pse, species_name), hoverinfo = 'text')
fig <- fig %>% 
  layout(scene = list(
    xaxis = list(title = "Biological"),
    yaxis = list(title = "Habitat"),
    zaxis = list(title = "Climate change")
  )) %>%
  add_markers() 
fig # Display the plot

#------------------------------------------------------------------------------
# Cluster analysis
#------------------------------------------------------------------------------
n_clust <- 5

#-----
# K-means
#-----
scaled_data <- scaled_data %>% arrange(biostatus, pop_score)

# Run K-means with n_clust
set.seed(123) # for reproducibility
kmeans_result <- kmeans(
  scaled_data %>% select(pop_score, hab_score, clim_score), 
  centers = n_clust, 
  nstart = 25)

# Add cluster assignments back to original data
scaled_data$k_cluster <- as.factor(kmeans_result$cluster)

# View results
print(kmeans_result$centers) # Interpretation of cluster characteristics

write.csv(kmeans_result$centers, "output/k-means_clusters.csv")

table(scaled_data$k_cluster)      # Size of each cluster

#------------------------------------------------------------------------------
# 3D plot by kmeans cluster
#------------------------------------------------------------------------------

scaled_data <- scaled_data %>% arrange(k_cluster, pop_score)

fig <- plot_ly(
  scaled_data, 
  x = ~pop_score, y = ~hab_score, z = ~clim_score, 
  color = ~k_cluster, colors = viridis(n = n_clust),
  text = ~paste(cu_name_pse, species_name), hoverinfo = 'text')
fig <- fig %>% 
  layout(scene = list(
    xaxis = list(title = "Biological"),
    yaxis = list(title = "Habitat"),
    zaxis = list(title = "Climate change")
  )) %>%
  add_markers() 
fig # Display the plot


#------------------------------------------------------------------------------
# 2D plot by kmeans cluster
#------------------------------------------------------------------------------
quartz(width = 8, height = 5.5, pointsize = 10)
par(mfrow = c(1,1),mar = c(4,4,2,2), oma = c(0,0,0,8))

# CUrrent recovery paradigm
strat <- data.frame(kmeans_result$centers) %>%
  mutate(cluster_no = c(1:5), 
         cluster_size = table(scaled_data$k_cluster),
         pop_score = round(pop_score, 3),
         hab_score = round(hab_score, 3),
         clim_score = round(clim_score, 3)) %>%
  select(cluster_no, pop_score, hab_score, clim_score, cluster_size)

plot_2Dhabbio(axis.lwd = 7)

for(k in 1:5){ # For each cluster
  scaled_data.k <- scaled_data %>% filter(k_cluster == k)
  hull_indices <- chull(scaled_data.k$hab_score, scaled_data.k$pop_score)
  hull_indices <- c(hull_indices, hull_indices[1])  # close the polygon

  polygon(x = scaled_data.k$hab_score[hull_indices], 
          y = scaled_data.k$pop_score[hull_indices],
          border = NA,
          col = paste0(clim_cols[findInterval(strat$clim_score[k], clim_breaks)], 80))
  lines(scaled_data.k$hab_score[hull_indices], scaled_data.k$pop_score[hull_indices], col = viridis(5)[k], lwd = 2, lty = 3)

}  
points(scaled_data$hab_score, scaled_data$pop_score, pch = as.numeric(scaled_data$k_cluster), lwd = 1.5, col = viridis(5)[scaled_data$k_cluster], cex = 1.5)
text(strat$hab_score, strat$pop_score, c(1:5), cex = 2, font = 2, col = viridis(5))

mtext(side = 3, line = 1, adj = 0, "b)", cex = 1.2)
mtext(side = 1, "Habitat status", line = 2.5, cex = 1.2)
mtext(side =2, "Biological status", line =2.5, cex = 1.2)


cluster_names <- c("Recovery", "Habitat\nrestoration", "Sustained\ninvestment", "Targeted\nrestoration", "Protection")
legend(2, 1, title = "Cluster", legend = cluster_names, pch = c(1:5), col = viridis(5), pt.cex = 1.5, lwd = 2, lty = NA, xpd = NA, cex =1.2, bty = "n")

quartz.save(file = "output/Fig1c.pdf", type = "pdf", width = 8, height = 5.5, pointsize = 10)
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


###############################################################################
# What's the optimal number of clusters?
###############################################################################

#-----
# Elbow method
#-----

library(ggplot2)

wss <- sapply(1:10, function(k) {
  kmeans(scaled_data_stripped, centers = k, nstart = 25)$tot.withinss
})

elbow_df <- data.frame(k = 1:10, wss = wss)

plot(elbow_df$k, elbow_df$wss, "o", xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster SS", bty = "l", las = 1)
abline(h = seq(10, 150, 10), col = grey(0.8))
abline(v = seq(1,10,1), col = grey(0.8))



ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Number of Clusters (k)", ylab = "Total Within-Cluster SS") +
  theme_minimal()

#-----
# Silhouette Method (Cluster Cohesion & Separation)
#-----
library(cluster)

sil_width <- sapply(2:10, function(k) {
  km <- kmeans(scaled_data_stripped, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(scaled_data))
  mean(sil[, 3])
})

sil_df <- data.frame(k = 2:10, sil_width = sil_width)

ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "Silhouette Method", x = "Number of Clusters (k)", y = "Average Silhouette Width") +
  theme_minimal()

#-----
# Gap Statistic (Compares to Random Baseline)
#-----

gap_stat <- clusGap(scaled_data_stripped, FUN = kmeans, K.max = 10, B = 50, nstart = 25)
plot(gap_stat, main = "Gap Statistic")

# Print the suggested optimal k
print(gap_stat, method = "firstSEmax")

library(factoextra)

fviz_nbclust(scaled_data_stripped, kmeans, method = "wss")       # Elbow
fviz_nbclust(scaled_data_stripped, kmeans, method = "silhouette") # Silhouette
fviz_nbclust(scaled_data_stripped, kmeans, method = "gap_stat")   # Gap statistic

###############################################################################
# Write to csv
###############################################################################
cluster_names <- c("Recovery", "Habitat\nrestoration", "Sustained\ninvestment", "Targeted\nrestoration", "Protection")
hab_data <- read.csv("data/dataset556_cu_habitat_threats.csv")

# Benchmarks
benchmarks0 <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset102_output")



data_out <- data %>%
  select(species_name, cuid, cu_name_pse, biostatus, biostatus_type, clim_score) %>%
  rename("climate_exposure" = "clim_score") %>%
  left_join(scaled_data %>% select(cuid, pop_score, hab_score, clim_score, k_cluster)) %>%
  mutate(conservation_type = case_when(
    k_cluster == 1 ~ "Recovery",
    k_cluster == 2 ~ "Habitat restoration",
    k_cluster == 3 ~ "Sustained investment",
    k_cluster == 4 ~ "Targeted restoration",
    k_cluster == 5 ~ "Protection")) %>%
  left_join(hab_data %>% select(cuid, cumulative_lopct, cumulative_modpct, cumulative_hipct)) %>% 
  left_join(benchmarks0 %>% select(cuid, curr_spw, curr_spw_end_year)) %>%
  select(species_name, cuid, cu_name_pse, biostatus, biostatus_type, curr_spw, curr_spw_end_year, cumulative_lopct, cumulative_modpct, cumulative_hipct, climate_exposure, pop_score, hab_score, clim_score, k_cluster, conservation_type)

write.csv(data_out, file = "output/TableS1_CUs.csv")
