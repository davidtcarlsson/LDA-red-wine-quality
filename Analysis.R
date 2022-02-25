# Load the packages
library("MVN")

# Load the dataset
df <- read.csv("./Datasets/winequality-red.csv")

# Assumptions for LDA
# - Data is multivariate normally distributed
# - Covariance matrices are equal for each class

# Lets test the assumption of normality
# Mardia's test
mvn(df, mvnTest = "mardia")
# Henze-Zirkler's test
mvn(df, mvnTest = "hz")
# Royston's test
mvn(df, mvnTest = "royston")

# Different univariate and multivariate plots
mvn(df, univariatePlot = "qqplot")
mvn(df, univariatePlot = "histogram")
par(mfrow=c(1,1))
mvn(df, multivariatePlot = "qq")

