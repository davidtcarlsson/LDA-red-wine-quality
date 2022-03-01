# Load the packages
library("biotools")
library("MVN")

# Load the dataset
df <- read.csv("./Datasets/penguins_size.csv")

# For this analysis we will exclude the "island"  and "sex" variable.
df <- subset(df, select = -c(island, sex))

# We find that we have two missing values in each of the independent variables
summary(df)

# We also find that the missing values are connected to two observations
df[is.na(df$culmen_length_mm), ]

# For this reason we find that imputation is not a good choice and would rather
# just remove the two observations.
df <- df[!is.na(df$culmen_length_mm), ]

# Split into train and test sets
perc_train <- 0.7
split <- sample(c(rep(0, perc_train * nrow(df)), rep(1, (1-perc_train) * nrow(df))))
train <- df[split == 0, ]   
test <- df[split == 1, ]  

# Assumptions for LDA
# - Data is multivariate normally distributed
# - Covariance matrices are equal for each class

# Lets test the assumption of normality
# Mardia's test
mvn(subset(df, select = -species), mvnTest = "mardia")
# Henze-Zirkler's test
mvn(subset(df, select = -species), mvnTest = "hz")
# Royston's test
mvn(subset(df, select = -species), mvnTest = "royston")

# Different univariate and multivariate plots
mvn(subset(df, select = -species), univariatePlot = "qqplot")
mvn(subset(df, select = -species), univariatePlot = "histogram")
par(mfrow=c(1,1))
mvn(subset(df, select = -species), multivariatePlot = "qq")

# Test for equal covariance matrices
# We will have to look at the covariance matrices manually since
# the test for equal covariances require the data to be multivariate
# normal.

# The model is defined as 
lda <- lda(species ~ ., data = train)
pred <- predict(lda, newdata = test)$class

# Where the confusion matrix is
table(predict(lda, newdata = test)$class, test$species)

# The hit rate can be calculated as
mean(predict(lda, newdata = test)$class == test$species) # ~99%

# The maximum criterion would give us a hit rate of
max(round(prop.table(table(test$species))*100, 1)) # ~41%

# The hit rate using proportional chance criterion would b
sum(prop.table(table(test$species))^2) # ~35%

# Since our hitrate of 99% is around 64 percentage points higher than random
# guessing we do not fulfill the rule of thumb from Hair et al. (2010).
