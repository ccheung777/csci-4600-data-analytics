##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)

## read dataset
# Ensure wine.data is in your working directory
wine <- read_csv("wine.data", col_names = FALSE)

## set column names - Using underscores to prevent subsetting errors
names(wine) <- c("Type","Alcohol","Malic_acid","Ash","Alcalinity_of_ash",
                 "Magnesium","Total_phenols","Flavanoids","Nonflavanoid_Phenols",
                 "Proanthocyanins","Color_Intensity","Hue",
                 "Od280_od315_diluted","Proline")

## change the data type of the "Type" column to factor
wine$Type <- as.factor(wine$Type)

X <- wine[,-1]
Y <- wine$Type

# PCA Computation 
wine_pca <- prcomp(X, center = TRUE, scale. = TRUE)

# Plot PCs 
autoplot(wine_pca, data = wine, colour = 'Type', 
         loadings = TRUE, loadings.label = TRUE, 
         loadings.label.size = 3, loadings.label.vjust = 1.5,
         main = "PCA: PC1 vs PC2")

# Identify top contributing variables to PC1
pc1_loadings <- sort(abs(wine_pca$rotation[,1]), decreasing = TRUE)
print("Top contributors to PC1:")
print(pc1_loadings)

# Train kNN: Original Variables (Subset) 
set.seed(123)
# Creating subset with Alcohol, Flavanoids, Color Intensity, and Proline
subset_features <- X[, c("Alcohol", "Flavanoids", "Color_Intensity", "Proline")]

# Split data 70/30
trainIndex <- createDataPartition(Y, p = 0.7, list = FALSE)
train_X_sub <- subset_features[trainIndex,]
test_X_sub  <- subset_features[-trainIndex,]
train_Y     <- Y[trainIndex]
test_Y      <- Y[-trainIndex]

# kNN Model 1
pred_knn_raw <- knn(train = train_X_sub, test = test_X_sub, cl = train_Y, k = 5)

# Train kNN: PCA Scores (PC1 & PC2) 
pc_scores   <- as.data.frame(wine_pca$x[, 1:2]) 
train_X_pca <- pc_scores[trainIndex,]
test_X_pca  <- pc_scores[-trainIndex,]

# kNN Model 2
pred_knn_pca <- knn(train = train_X_pca, test = test_X_pca, cl = train_Y, k = 5)

# Evaluation & Comparison 
cat("\n Contingency Table: Subset Raw Features \n")
print(table(Predicted = pred_knn_raw, Actual = test_Y))

cat("\n Contingency Table: PC1 & PC2 Scores \n")
print(table(Predicted = pred_knn_pca, Actual = test_Y))

# Metrics calculation
cm_raw <- confusionMatrix(pred_knn_raw, test_Y)
cm_pca <- confusionMatrix(pred_knn_pca, test_Y)

cat("\n Performance Comparison \n")
metrics_comp <- data.frame(
  Model = c("Raw Subset", "PCA (PC1-2)"),
  Accuracy = c(cm_raw$overall['Accuracy'], cm_pca$overall['Accuracy']),
 # Macro-averaging metrics across the three wine types
  Precision = c(mean(cm_raw$byClass[,'Precision'], na.rm=TRUE), mean(cm_pca$byClass[,'Precision'], na.rm=TRUE)),
  Recall = c(mean(cm_raw$byClass[,'Recall'], na.rm=TRUE), mean(cm_pca$byClass[,'Recall'], na.rm=TRUE)),
  F1_Macro = c(mean(cm_raw$byClass[,'F1'], na.rm=TRUE), mean(cm_pca$byClass[,'F1'], na.rm=TRUE))
)
print(metrics_comp)

#EOF
