# Load the packages
library(biotools) # Used to train the lda model
library(MVN) # Used to the multivariate normality tests
library(ggplot2) # Used to create our plots
library(GGally) # Used to create the pair plot
library(ggExtra) # Used for the marginal plots
library(caret) # Used to evaluate model

# Set the  base theme for ggplot2
theme_set(theme_classic(base_size = 14))

# Set a seed so that our train/test sets stay the same
set.seed(1234)

# Load the dataset
df <- read.csv("./Datasets/penguins_size.csv")

# For this analysis we will exclude the "island"  and "sex" variable.
df <- subset(df, select = -c(island, sex))
colnames(df) <- c("Art", "Culmenlängd", "Culmendjup", "Vinglängd", "Vikt")

# We find that we have two missing values in each of the independent variables
summary(df[df$Art == "Gentoo", ])
summary(df[df$Art == "Chinstrap", ])
summary(df[df$Art == "Adelie", ])

# We also find that the missing values are connected to two observations
df[is.na(df$Culmenlängd), ]

# For this reason we find that imputation is not a good choice and would rather
# just remove the two observations.
df <- df[!is.na(df$Culmenlängd), ]

# The standard deviations of our variables are
round(sapply(df[df$Art == "Gentoo", ][2:5], mean), 1)
round(sapply(df[df$Art == "Gentoo", ][2:5], sd), 1)

round(sapply(df[df$Art == "Chinstrap", ][2:5], mean), 1)
round(sapply(df[df$Art == "Chinstrap", ][2:5], sd), 1)

round(sapply(df[df$Art == "Adelie", ][2:5], mean), 1)
round(sapply(df[df$Art == "Adelie", ][2:5], sd), 1)

round(sapply(df[2:5], mean), 1)
round(sapply(df[2:5], sd), 1)

# Frequency plot
ggplot(data = df, aes(x = Art, fill = Art)) +
  geom_bar() +
  ylab("Antal") +
  labs(title=paste("Antal pingviner (n=", nrow(df), ")", sep="")) +
  geom_text(aes(label=paste(..count.., " (", round(..count../nrow(df)*100, 1), "%)", sep="")), 
            stat="count", nudge_y = 4) +
  theme(legend.position = "none")
ggsave(filename = "freq_plot.png", path = "./Plots", 
       height = 5, width = 5)

# Split into train and test sets
perc_train <- 0.7
split <- sample(c(rep(0, perc_train * nrow(df)), rep(1, (1-perc_train) * nrow(df))))
train <- df[split == 0, ]   
test <- df[split == 1, ]  

# Summary plot for all independent variables
ggpairs(df, columns = 2:5, aes(color = Art, alpha = 0.05))
ggsave(filename = "var_summary.png", path = "./Plots", 
       height = 9, width = 9)

# Assumptions for LDA
# - Data is multivariate normally distributed
# - Covariance matrices are equal for each class

# Lets test the assumption of normality
# Mardia's test
mvn(subset(df, select = -Art), mvnTest = "mardia")$multivariateNormality
# Henze-Zirkler's test
mvn(subset(df, select = -Art), mvnTest = "hz")$multivariateNormality
# Royston's test
mvn(subset(df, select = -Art), mvnTest = "royston")$multivariateNormality

# Add these in tables on google docs.

# Different univariate and multivariate plots
mvn(subset(df, select = -Art), univariatePlot = "qqplot")
mvn(subset(df, select = -Art), univariatePlot = "histogram")
par(mfrow=c(1,1))
mvn(subset(df, select = -Art), multivariatePlot = "qq")

# Test for equal covariance matrices
# We will have to look at the covariance matrices manually since
# the test for equal covariances require the data to be multivariate
# normal.
round(cov(df[df$Art == "Gentoo", ][, 2:5]), 1)
round(cov(df[df$Art == "Chinstrap", ][, 2:5]), 1)
round(cov(df[df$Art == "Adelie", ][, 2:5]), 1)

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
lda_plot <- ggplot(lda_output_test, aes(x = LD1, y = LD2, color=Art)) +
  geom_point(size = 4, alpha=0.6) + 
  theme(text = element_text(family = "Lato"), legend.position = "bottom")
ggMarginal(lda_plot, type="histogram", groupFill = TRUE, 
           xparams = list(bins=c(25)),
           yparams = list(bins=c(25)))
#ggsave(filename = "lda_output_2d.png", plot = lda_plot, path = "./Plots", 
#       height = 15, width = 15)

# Lets look at the one dimensional plots
# One dimensional plot for LD1
# ggplot(lda_output_test, aes(x=x, y=LD1, color=Art)) +
#   geom_jitter(position=position_jitter(0.2), size = 3, alpha=0.6) + 
#   xlab("") +
#   xlim(-2, 2)  +
#   theme(text = element_text(family = "Lato"))

# One dimensional plot for LD2
# ggplot(lda_output_test, aes(x=x, y=LD2, color=Art)) +
#   geom_jitter(position=position_jitter(0.2), size = 3, alpha=0.6) + 
#   xlab("") +
#   xlim(-2, 2)  +
#   theme(text = element_text(family = "Lato"))

# We get a summary of our evaluation metrics using
eval_metrics <- confusionMatrix(table(lda_output_test$Art, test$Art))
eval_metrics$table # Confusion matrix
round(eval_metrics$overall["Accuracy"]*100, 1) # Hitrate

# The maximum criterion would give us a hit rate of
max(round(prop.table(table(test$Art))*100, 1)) 

# The hit rate using proportional chance criterion would b
round(sum(prop.table(table(test$Art))^2)*100, 1)

