# install the required packages if they don't already exist
if (!requireNamespace("factoextra", quietly=TRUE))install.packages("factoextra")
if (!requireNamespace("cluster", quietly=TRUE))install.packages("cluster")


# Load the libraries
library(factoextra)
library(cluster)

# load data
winequality_data_set <- read.table('C:/Users/USER/Documents/FH Technikum/R Tutorials/winequality-red.csv', sep=";", header=TRUE)

# remove rows with missing values
# This line of code is optional. You can decide to write it or not. It all depends on how well you trust the
# datasets to not have any missing value. In my case, I decided to just write it.
winequality_data_set <- na.omit(winequality_data_set)

# scale each variable to have a mean of 0 and sd of 1
# The scale() function is centering and scaling the data! This is particularly useful when working with algorithm
# that are sensitive to the scaling of variables 

winequality_data_set_scaled <- scale(winequality_data_set)

# view the first 6 rows of the dataset
# winequality_data_set <- head(winequality_data_set)
# print(winequality_data_set)

# Since we don't know beforehand which method will produce the best clusters, we will write a short function
# that will perform the hierarchical clustering using several different methods.
# Note that this function calculates the agglomerative coefficient of each method, which is a metric that measures
# the strength of the clusters. The closer this value is to 1, the stronger the clusters.

# define the linkage methods
wine_quality <- c("average", "single", "complete", "ward")
names(wine_quality) <- c("average", "single", "complete", "ward")

# function to compute agglomerative coefficient
quality_red <- function(x) {
    agnes(winequality_data_set_scaled, method = x)$ac
}

# calculate agglomerative coefficient for each clustering linkage method
agglo_quality <- sapply(wine_quality, quality_red)
print(agglo_quality)

# Based on the `print(agglo_quality)`, we can see that the Ward's minimum variance method produces the highest
# agglomerative coefficient, thus we will use that as the method for our final hierarchical clustering.

# perform hierarchical clustering using Ward's minimum variance 
hierarchical_quality <- agnes(winequality_data_set_scaled, method = "ward")

# Plot the dendrogram
hq <- pltree(hierarchical_quality, cex = 0.6, hang = -1, main = "Dendrogram - Ward's Method")

# Each leaf at the bottom of the dendrogram represents an observation in the original dataset. As we move up
# the dendrogram from the bottom, observations that are similar to each other are fused together in a branch.

# To determine how many clusters the observations should be grouped in, we would use a metric known as the 
# `gap statistics`, which compares the total intra-cluster variation for different values of k with their 
# expected values for a distribution with no clustering.

# calculate gap statistic for each number of clusters (up to 10 clusters)
set.seed(123)
quality_stats <- clusGap(winequality_data_set_scaled, FUN = hcut, nstart = 25, K.max = 10, B = 50)

# produce plot of clusters vs. gap statistic
fviz_gap_stat(quality_stats)

# Determine the optimal number of clusters
gap_df <- as.data.frame(quality_stats$Tab)
optimal_k <- which.max(gap_df$gap)
print(paste("Optimal number of clusters:", optimal_k))

# Perform final hierarchical clustering using Ward's method
final_clust <- hclust(dist(winequality_data_set_scaled, method = "euclidean"), method = "ward.D2")

# Cut the dendrogram into the optimal number of clusters
cluster_wine <- cutree(final_clust, k = optimal_k)
print(table(cluster_wine))

# Append cluster labels to original data
final_data <- as.data.frame(cbind(winequality_data_set, cluster = cluster_wine))

# Display first six rows of the final data
head(final_data)

# Find mean values for each cluster
cluster_means <- aggregate(. ~ cluster, data = final_data, mean)
print(cluster_means)