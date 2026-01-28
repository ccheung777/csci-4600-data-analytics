library(readr)
library(EnvStats)
library(nortest)

# set working directory (relative path)
setwd("/Users/celine/Downloads/CSCI 4600 Data Analytics/csci-4600-data-analytics")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

##### Variable Summary #####
# Focusing on the variables ECS.new and FSS.new
ECS <- epi.data$ECS.new
FSS <- epi.data$FSS.new

# Clean the varaiables by removing the N/A values
ECS.cleaned <- ECS[!is.na(ECS)]
FSS.cleaned <- FSS[!is.na(FSS)]

# Print the variable summaries
print("ECS Summary:")
summary(ECS.cleaned)

print("FSS Summary:")
summary(FSS.cleaned)

##### Variable Boxplots #####
boxplot(ECS.cleaned, FSS.cleaned,
        names = c("ECS.new", "FSS.new"), 
        main = "ECS.new and FSS.new Boxplot Comparison", 
        col = c("lightblue", "lightgreen"))

##### Histograms with overlayed theoretical probability distributions #####
# ECS.new Histogram
ecs_x <- seq(min(ECS.cleaned), max(ECS.cleaned), length=100)
hist(ECS.cleaned, 
     prob=TRUE, 
     main="ECS.new Histogram",
     xlab="ECS.new Score", 
     col ="lightblue")
lines(density(ECS.cleaned,
              bw="SJ"))
# ECS.new Overlay Theoretical Normal Distribution (bell curve based on data's mean/sd)
ecs_mean <- mean(ECS.cleaned)
ecs_sd <- sd(ECS.cleaned)
ecs_y <- dnorm(ecs_x, mean=ecs_mean, sd=ecs_sd)
lines(ecs_x, ecs_y, col="red", lty=2, lwd=2)

# FSS.new Histogram
fss_x <-seq(min(FSS.cleaned), max(FSS.cleaned), length=100)
hist(FSS.cleaned,
     prob=TRUE,
     main="FSS.new Histogram", 
     xlab= "FSS.new Score", 
     col="lightgreen")
lines(density(FSS.cleaned,
              bw="SJ"))
# FSS.new Overlay Theoretical Normal Distribution (bell curve based on data's mean/sd)
fss_mean = mean(FSS.cleaned)
fss_sd = sd(FSS.cleaned)
fss_y = dnorm(fss_x, mean=fss_mean, sd=fss_sd)
lines(fss_x, fss_y, col="red", lty=2, lwd=2)

##### ECDF Plots #####
plot(ecdf(ECS.cleaned), do.points=FALSE, verticals=TRUE)
plot(ecdf(FSS.cleaned), do.points=FALSE, verticals=TRUE)

##### QQ plots of each variable against the normal distribution #####
# ECS against Normal
par(mfrow=c(1,2))
qqnorm(ECS.cleaned, main="ECS.new QQ Plots"); qqline(ECS.cleaned, col="red")

# FSS against Normal
qqnorm(FSS.cleaned, main="FSS.new QQ Plots"); qqline(FSS.cleaned, col="red")
par(mfrow=c(1,1))

##### QQ Plot of the 2 variables against each other #####
qqplot(ECS.cleaned, FSS.cleaned, 
       xlab = "ECS.new Quantiles", 
       ylab = "FSS.new Quantiles",
       main = "ECS.new vs FSS.new QQ Plot")
# Reference line (y=x) to check if distributions are identical
abline(0, 1, col="red")

##### Normality Statistical Tests For Each Variable #####
# Shapiro-Wilk
print("Shapiro Test for ECS.new")
shapiro.test(ECS.cleaned)
print("Shapiro Test for FSS.new")
shapiro.test(FSS.cleaned)
# Anderson-Darling
print("Anderson-Darling Test for ECS.new")
ad.test(ECS.cleaned)
print("Anderson-Darling Test for FSS.new")
ad.test(FSS.cleaned)

##### Statistical Test for Identical Distributions #####
# Kolmogorov-Smirnov Test
print("Kolmogorov-Smirnov Test (ECS vs FSS):")
ks.test(ECS.cleaned, FSS.cleaned)

# The p-value of ks.test < 0.05 indicating that the distributions 
# of ECS.new and FSS.new are statistically different

# Wilcoxon Rank Sum Test (Non-parametric comparison of means/distributions)
print("Wilcoxon Test (ECS vs FSS):")
wilcox.test(ECS.cleaned, FSS.cleaned)