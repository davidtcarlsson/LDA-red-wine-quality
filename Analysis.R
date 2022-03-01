# Load the packages
library(biotools)
library(MVN)
library(ggplot2)
library(GGally)
library(psych)

# Load the dataset
df <- read.csv("./Datasets/penguins_size.csv")

# For this analysis we will exclude the "island"  and "sex" variable.
df <- subset(df, select = -c(island, sex))
colnames(df) <- c("Art", "Culmenlängd", "Culmendjup", "Vinglängd", "Vikt")

# We find that we have two missing values in each of the independent variables
summary(df)

# We also find that the missing values are connected to two observations
df[is.na(df$Culmenlängd), ]

# For this reason we find that imputation is not a good choice and would rather
# just remove the two observations.
df <- df[!is.na(df$Culmenlängd), ]

# Split into train and test sets
perc_train <- 0.9
split <- sample(c(rep(0, perc_train * nrow(df)), rep(1, (1-perc_train) * nrow(df))))
train <- df[split == 0, ]   
test <- df[split == 1, ]  

# Summary plot for all independent variables
ggpairs(df, columns = 2:5, aes(color = Art, alpha = 0.05))

# Lets look at the correlations
corPlot(df[, 2:5]) 

# Assumptions for LDA
# - Data is multivariate normally distributed
# - Covariance matrices are equal for each class

# Lets test the assumption of normality
# Mardia's test
mvn(subset(df, select = -Art), mvnTest = "mardia")
# Henze-Zirkler's test
mvn(subset(df, select = -Art), mvnTest = "hz")
# Royston's test
mvn(subset(df, select = -Art), mvnTest = "royston")

# Different univariate and multivariate plots
mvn(subset(df, select = -Art), univariatePlot = "qqplot")
mvn(subset(df, select = -Art), univariatePlot = "histogram")
par(mfrow=c(1,1))
mvn(subset(df, select = -Art), multivariatePlot = "qq")

# Test for equal covariance matrices
# We will have to look at the covariance matrices manually since
# the test for equal covariances require the data to be multivariate
# normal.
cov(df[df$Art == "Gentoo", ][, 2:5])
cov(df[df$Art == "Chinstrap", ][, 2:5])
cov(df[df$Art == "Adelie", ][, 2:5])

# The model is defined as 
lda <- lda(Art ~ ., data = train)

# Data used to plot our lda results
lda_pred_train <- predict(lda)
lda_pred_test <- predict(lda, newdata = test)

# Reset the plot surface
par(mfrow = c(1, 1))

# Lets create two dataframes with useful data to plot
lda_output_train <- data.frame(
  'LD1' = lda_pred_train$x[, 1],
  'LD2' = lda_pred_train$x[, 2],
  "Art" = lda_pred_train$class, 
  x = 0)

lda_output_test <- data.frame(
  'LD1' = lda_pred_test$x[, 1],
  'LD2' = lda_pred_test$x[, 2],
  "Art" = lda_pred_test$class, 
  x = 0)

# Plot on the test data
ggplot(lda_output_test, aes(x = LD1, y = LD2, color=Art)) +
  geom_point()

# Lets look at the one dimensional plots
# One dimensional plot for LD1
ggplot(lda_output_test, aes(x=x, y=LD1, color=Art)) +
  geom_jitter(position=position_jitter(0.2)) + 
  xlab("") +
  xlim(-2, 2) 

# One dimensional plot for LD2
ggplot(lda_output_test, aes(x=x, y=LD2, color=Art)) +
  geom_jitter(position=position_jitter(0.2)) + 
  xlab("") +
  xlim(-2, 2) 

# Where the confusion matrix is
table(lda_output_test$Art, test$Art)

# The hit rate can be calculated as
mean(lda_output_test$Art == test$Art) # ~100%

# The maximum criterion would give us a hit rate of
max(round(prop.table(table(test$Art))*100, 1)) 

# The hit rate using proportional chance criterion would b
sum(prop.table(table(test$Art))^2) 
