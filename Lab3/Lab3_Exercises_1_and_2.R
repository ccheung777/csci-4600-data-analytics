# Celine Cheung 662036121
# CSCI 4960 Data Analytics Spring 2026
# Lab 3 Exercises 1 and 2

library(caret)
library(class)
library(cluster)

# Read Data
abalone <- read.csv('/Users/celine/Downloads/CSCI 4600 Data Analytics/Labs/Lab 3/abalone/abalone.data', header=FALSE)
# Rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 

# Derive age group based in number of rings
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

# Take copy removing sex and rings
abalone.sub <- abalone[,c(2:8,10)]

# Convert class labels to strings
abalone.sub$age.group <- as.character(abalone.sub$age.group)

# Convert back to factor
abalone.sub$age.group <- as.factor(abalone.sub$age.group)

# Split train/test but lock in the results
set.seed(123)
train.indexes <- sample(4177,0.7*4177)
train <- abalone.sub[train.indexes,]
test <- abalone.sub[-train.indexes,]


# ----- Exercise 1 ----- 
# Subsets of features
features_1 <- c("length", "diameter", "height")
features_2 <- c("whole_weight", "shucked_wieght", "viscera_wieght", "shell_weight")

# 2 kNN models, using k=5 as starting point
knn1_pred <- knn(train = train[, features_1], test = test[, features_1], cl = train$age.group, k = 5)
knn2_pred <- knn(train = train[, features_2], test = test[, features_2], cl = train$age.group, k = 5)

# Model 1 + Contingency Table
print("Model 1: Physical Dimensions")
cm_1 <-table(Actual = test$age.group, Predicted = knn1_pred)
print(cm_1)
acc_1 <- sum(diag(cm_1)) / sum(cm_1)
cat("Accuracy:", round(acc_1, 4), "\n")

# Model 2 + Contingency Table
print("Model 2: Weight Measurements")
cm_2 <- table(Actual = test$age.group, Predicted = knn2_pred)
print(cm_2)
acc_2 <- sum(diag(cm_2)) / sum(cm_2)
cat("Accuracy:", round(acc_2, 4), "\n")

best_features <- if (acc_1 > acc_2) features_1 else features_2

# Finding the optimal k for better performing model
print("Optimizing k for the better model")
cat("Using features:", paste(best_features, collapse=", "), "\n")

k_values <- 1:40
accuracies <-numeric(length(k_values))

for (i in seq_along(k_values)){
  k <- k_values[i]
  preds <- knn(train = train[, best_features],
               test = test[, best_features],
               cl = train$age.group,
               k = k)
  accuracies[i] <- sum(preds == test$age.group) / length(preds)
}

best_k <- k_values[which.max(accuracies)]
max_acc <- max(accuracies)

cat("Tested k values from 1 to 40.\n")
cat("Optimal k:", best_k, "\n")
cat("Highest Accuracy:", round(max_acc, 4), "\n")

# Plot the accuracy over the range of k values
plot(k_values, accuracies, type="b", col="blue", pch=19,
     xlab="k (Number of Neighbors)", ylab="Accuracy", 
     main="Optimization of k for best kNN model")

# ----- Exercise 2 ----- 
# Use best_features from Exercise 1
clust_data <- abalone.sub[, best_features]
scaled_clust_data <- scale(clust_data)

# Average Silhouette Width, test k values from 2-10
k_range <- 2:10
avg_sil_kmeans <- numeric(length(k_range))
avg_sil_pam <- numeric(length(k_range))

print("Calculating optimal k for K-Means and PAM")
dist_matrix <- dist(scaled_clust_data)

for(i in seq_along(k_range)){
  k <- k_range[i]

  km_res <- kmeans(scaled_clust_data, centers=k, nstart=25)
  sil_km <- silhouette(km_res$cluster, dist_matrix)
  avg_sil_kmeans[i] <- mean(sil_km[, 3])
  
  pam_res <- pam(scaled_clust_data, k = k)
  avg_sil_pam[i] <- pam_res$silinfo$avg.width
}

# Finding optimal k for each model
optimal_k_kmeans <- k_range[which.max(avg_sil_kmeans)]
optimal_k_pam <- k_range[which.max(avg_sil_pam)]

cat("Optimal k for K-Means:", optimal_k_kmeans, "\n")
cat("Optimal k for PAM:", optimal_k_pam, "\n")

# Final Models & Silhouette Plots
par(mfrow = c(1, 2))

final_kmeans <- kmeans(scaled_clust_data, centers = optimal_k_kmeans, nstart = 25)
sil_final_kmeans <- silhouette(final_kmeans$cluster, dist_matrix)

plot(sil_final_kmeans,
     main = paste("K-Means Silhouette (k =", optimal_k_kmeans, ")"), 
     col = 1:optimal_k_kmeans, border = NA)

final_pam <- pam(scaled_clust_data, k=optimal_k_pam)
plot(silhouette(final_pam),
     main=paste("PAM Silhouette (k=", optimal_k_pam, ")"),
     col = 1:optimal_k_pam, border= NA)
par(mfrow = c(1,1))

# End of Script