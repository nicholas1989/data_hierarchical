# Install the required packages if they don't already exist
if(!requireNamespace("ggplot2", quietly=TRUE)) install.packages("ggplot2")
if(!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
if(!requireNamespace("dplyr", quietly=TRUE)) install.packages("dplyr")
if(!requireNamespace("readr", quietly=TRUE)) install.packages("readr")
if(!requireNamespace("cluster", quietly=TRUE)) install.packages("cluster")

# Load the libraries
library(ggplot2)
library(factoextra)
library(dplyr)
library(readr)
library(cluster)

# Load the data
winequality_data_set <- read.table('C:/Users/USER/Documents/FH Technikum/R Tutorials/winequality-red.csv', sep=";", header=TRUE)

# We check the structure of the dataset using `str()` function
str(winequality_data_set)

# We scale the dataset so as to standardize the 
# The scale() function is centering and scaling the data! This is particularly useful when working with algorithm
# that are sensitive to the scaling of variables 
scaled_winequality_data_set <- as.data.frame(scale(winequality_data_set))

# Perform PCA
wine_pca <- prcomp(scaled_winequality_data_set, center = TRUE, scale = TRUE)

# Summary of PCA to get the proportion of variance explained by each component
print(summary(wine_pca))

# Get the eigenvalues
eigenvalues <- get_eigenvalue(wine_pca)
print(eigenvalues)


# Scree plot to visualize the explained variance
fviz_eig(wine_pca, addlabels = TRUE, ylim = c(0, 50))


# Biplot to visualize PCA results
fviz_pca_biplot(wine_pca, repel = TRUE,
                col.var = "contrib", # Color by contributions to the PC
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = "Contrib")


# Get the coordinates of individuals
individuals <- as.data.frame(wine_pca$x)
print(head(individuals))


# Since we don't know beforehand which method will produce the best clusters, we will write a short function
# that will perform the hierarchical clustering using several different methods.
# Note that this function calculates the agglomerative coefficient of each method, which is a metric that measures
# the strength of the clusters. The closer this value is to 1, the stronger the clusters.
calculate_agglomerative_coefficient <- c("average", "single", "complete", "ward")
names(calculate_agglomerative_coefficient) <- c("average", "single", "complete", "ward")

# function to compute agglomerative coefficient
quality_red <- function(x) {
    agnes(individuals, method = x)$ac
}

# Calculate agglomerative coefficients for each method
ag_coefficients <- sapply(calculate_agglomerative_coefficient, quality_red)
print(ag_coefficients)

# Based on the `print(sapply(calculate_agglomerative_coefficient, quality_red))`, we can see that the Ward's minimum variance method produces the highest
# agglomerative coefficient, thus we will use that as the method for our final hierarchical clustering.
best_method <- names(which.max(ag_coefficients))
print(paste("Best method:", best_method))

# perform hierarchical clustering using Ward's minimum variance 
hierarchical_quality <- agnes(individuals, method = best_method)

# Convert agnes object to a dendrogram
hc_dendrogram <- as.dendrogram(hierarchical_quality)

# Plot the dendrogram
plot(hc_dendrogram, cex = 0.6, main = "Dendrogram - Ward's Method")

# To determine how many clusters the observations should be grouped in, we would use a metric known as the 
# `gap statistics`, which compares the total intra-cluster variation for different values of k with their 
# expected values for a distribution with no clustering.

# calculate gap statistic for each number of clusters (up to 10 clusters)
set.seed(123)
quality_stats <- clusGap(individuals, FUN = hcut, nstart = 25, K.max = 10, B = 50)

# produce plot of clusters vs. gap statistic
fviz_gap_stat(quality_stats)

# Determine the optimal number of clusters
gap_df <- as.data.frame(quality_stats$Tab)
optimal_k <- which.max(gap_df$gap)
print(paste("Optimal number of clusters:", optimal_k))

# Perform final hierarchical clustering using Ward's method
final_clust <- hclust(dist(individuals, method = "euclidean"), method = "ward.D2")

# Cut the dendrogram into the optimal number of clusters
cluster_wine <- cutree(final_clust, k = optimal_k)
print(table(cluster_wine))

# Add cluster results to the PCA data
individuals$cluster <- factor(cluster_wine)

# Generate a color palette with enough colors for all clusters
palette_colors <- colorRampPalette(c("#00AFBB", "#E7B800", "#FC4E07"))(optimal_k)

# Visualize the clusters on the PCA plot
print(ggplot(individuals, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Clusters identified by Hierarchical Clustering on PCA components",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  scale_color_manual(values = palette_colors))