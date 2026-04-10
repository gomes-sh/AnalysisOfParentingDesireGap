# Run this file third.
library(cluster)
library(clValid)
library(fpc)

names(c3)[c(1, 2, 5, 8, 13, 14, 15, 18, 20, 21, 22, 23, 24, 25, 31)]
# Setting aside top 10 variables determined by gini index and accuracy.
profiles <- c3[, c(1, 2, 5, 8, 13, 14, 15, 18, 20, 21, 22, 23, 24, 25, 31)]

# hclust to find profiles.
gower.dist <- daisy(profiles, metric = "gower")
hclust.01 <- hclust(gower.dist, method = "complete")

maxclusters <- 10

# Evaluating cluster metrics for first 10 clusters - See TablesGraphs for details.
clust.metrics <- data.frame(k = 2:maxclusters,
                            dunns = rep(0, maxclusters-1),
                            avg.sil = rep(NA, maxclusters-1),
                            b.w.ratio = rep(NA, maxclusters-1))

for(i in 2:maxclusters){
  results <- cluster.stats(d = gower.dist, clustering = cutree(tree = hclust.01, k = i))
  clust.metrics$dunns[i-1] <- results$dunn
  clust.metrics$avg.sil[i-1] <- results$avg.silwidth
  clust.metrics$b.w.ratio[i-1] <- results$average.between/(results$average.between+results$average.within)
}

metrics2 <- pivot_longer(data = clust.metrics, cols = !k, names_to = "Metric", values_to = "Value")


# Cutting the hierarchical tree to 2 clusters.
d.desc <- profiles

names(d.desc) <- c("Cohort", "Education", "Miles from LGBT Health",
                   "Good: GLB", "General Health", "30 Days: Physical Health",
                   "30 Days: Mental Health", "Out: Co-workers", "Community Connectedness",
                   "Everyday discrimination", "Felt Stigma", "Internalized Homophobia",
                   "Social Well-being", "Social Support - Family", "Parenting Gap")

d.desc$Cluster <- as.factor(cutree(tree = hclust.01, k = 2))
