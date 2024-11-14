# ========================================================================================================
# Purpose:      Final Project (Team) - Clustering Part
# Author:       Sasha A.
# DOC:          25/07/2024
# Topics:       K-Means Clustering, data table, boxplot analysis
# Data Source:  BostonHousing2
#=========================================================================================================

# Load necessary libraries
library(mlbench)
library(data.table)
library(ggplot2)
library(plotly)

# Load the Boston dataset
data("BostonHousing2")
boston_dt <- as.data.table(BostonHousing2)

# Inspect the dataset
head(boston_dt)
summary(boston_dt)

# Check for missing values
sum(is.na(boston_dt))

# Select features for clustering
clustering_data <- boston_dt[, .(lstat, rm, ptratio, cmedv)]
clustering_data

# Standardize the features (excluding 'cmedv')
clustering_data_scaled <- scale(clustering_data[, .(lstat, rm, ptratio)])

# Elbow method for determining the optimal number of clusters
wss <- (nrow(clustering_data_scaled) - 1) * sum(apply(clustering_data_scaled, 2, var))
for (i in 2:10) {
  set.seed(123)
  wss[i] <- sum(kmeans(clustering_data_scaled, centers = i, nstart = 25)$tot.withinss)
}

# Plot the elbow curve
elbow_plot <- ggplot(data = data.frame(k = 1:10, wss = wss), aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  ggtitle("Elbow Method for Determining Optimal k") +
  xlab("Number of Clusters (k)") +
  ylab("Within-Cluster Sum of Squares (WSS)") +
  theme_minimal()

print(elbow_plot)

# Perform K-means clustering (assuming optimal k is determined to be 3)
set.seed(123)  # For reproducibility
k <- 3
kmeans_result <- kmeans(clustering_data_scaled, centers = k, nstart = 25)

# Add cluster assignment to the original data
clustering_data[, cluster := as.factor(kmeans_result$cluster)]

# Create a 3D scatter plot using plotly
plot_ly(data = clustering_data, 
        x = ~lstat, 
        y = ~rm, 
        z = ~ptratio, 
        color = ~cluster, 
        colors = c('red', 'green', 'blue'),
        type = 'scatter3d', 
        mode = 'markers') %>%
  layout(scene = list(xaxis = list(title = 'lstat'),
                      yaxis = list(title = 'rm'),
                      zaxis = list(title = 'ptratio')),
         title = 'Cluster Visualization of Boston Housing Segregation')

# Boxplot for 'lstat'
ggplot(clustering_data, aes(x = cluster, y = lstat, fill = cluster)) +
  geom_boxplot() +
  ggtitle("Percentage of Lower-Status Population by Cluster") +
  xlab("Cluster") +
  ylab("Lower-Status Population (%)") +
  theme_minimal()

# Boxplot for 'rm'
ggplot(clustering_data, aes(x = cluster, y = rm, fill = cluster)) +
  geom_boxplot() +
  ggtitle("Average Number of Rooms by Cluster") +
  xlab("Cluster") +
  ylab("Average Number of Rooms per Dwelling") +
  theme_minimal()

# Boxplot for 'ptratio'
ggplot(clustering_data, aes(x = cluster, y = ptratio, fill = cluster)) +
  geom_boxplot() +
  ggtitle("Pupil-Teacher Ratio by Cluster") +
  xlab("Cluster") +
  ylab("Pupil-Teacher Ratio by Cluster") +
  theme_minimal()

# Calculate median housing prices per cluster
median_prices <- clustering_data[, .(Median_Price = median(cmedv)), by = cluster]

# Show the median prices
print(median_prices)

# Create a boxplot of median housing prices
ggplot(clustering_data, aes(x = cluster, y = cmedv, fill = cluster)) +
  geom_boxplot() +
  ggtitle("Boxplot of Housing Prices by Cluster") +
  xlab("Cluster") +
  ylab("Housing Price in USD thousands (cmedv)") +
  theme_minimal()
