# Celine Cheung 662036121
# CSCI 4600 Lab 2 
# Spring 2026

library("ggplot2")
library("readr")

## 1. Read Dataset
NY_House_Dataset <- read_csv("/Users/celine/Downloads/CSCI 4600 Data Analytics/Labs/Lab2/Lab 2/NY-House-Dataset.csv")
dataset <- NY_House_Dataset

## 2. Data Cleaning & Transformation
# Filter out the known extreme price outlier and the specific sqft anomaly
dataset <- dataset[dataset$PRICE < 195000000, ]
dataset <- dataset[dataset$PROPERTYSQFT != 2184.207862, ]

# Filter out any 0 values for price or sqft to prevent -Inf errors during log10 transformation
dataset <- dataset[dataset$PROPERTYSQFT > 0 & dataset$PRICE > 0, ]


# Model 1: Price ~ PropertySqFt + Beds
model1 <- lm(log10(PRICE) ~ log10(PROPERTYSQFT) + BEDS, data = dataset)

# Summary stats
print("Model 1 Summary:")
summary(model1)

# PropertySqFt vs Price with best fit line scatter plot
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point(alpha = 0.4) +
  stat_smooth(method = "lm", col="red") +
  ggtitle("Model 1: log10(Price) vs log10(PropertySqFt)")

# Residuals scatter plot
ggplot(model1, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, col="red", linetype="dashed") +
  ggtitle("Model 1 Residuals")


# Model 2: Price ~ Beds + Bath
model2 <- lm(log10(PRICE) ~ BEDS + BATH, data = dataset)

# Summary stats
print("Model 2 Summary:")
summary(model2)

# Price vs Bath with best fit line scatter plot
ggplot(dataset, aes(x = BATH, y = log10(PRICE))) +
  geom_point(alpha = 0.4) +
  stat_smooth(method = "lm", col="blue") +
  ggtitle("Model 2: log10(Price) vs Bath")

# Residuals scatter plot
ggplot(model2, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, col="red", linetype="dashed") +
  ggtitle("Model 2 Residuals")


# Model 3: Price ~ PropertySqFt + Beds + Bath
model3 <- lm(log10(PRICE) ~ log10(PROPERTYSQFT) + BEDS + BATH, data = dataset)

# Summary Stats
print("Model 3 Summary:")
summary(model3)

# PropertySqFt vs Price with best fit line scatter plot
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point(alpha = 0.4) +
  stat_smooth(method = "lm", col="purple") +
  ggtitle("Model 3: log10(Price) vs log10(PropertySqFt)")

# Residuals scatter plot
ggplot(model3, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, col="red", linetype="dashed") +
  ggtitle("Model 3 Residuals")

# End of Lab 2

