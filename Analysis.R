# Setup ####

## Load the packages ####
library(biotools) # Used to train the lda model
library(MVN) # Used to the multivariate normality tests
library(ggplot2) # Used to create our plots
library(GGally) # Used to create the pair plot
library(ggExtra) # Used for the marginal plots
library(caret) # Used to evaluate model
library(klaR) # Used to create a partition plot

# Set the  base theme for ggplot2 
theme_set(theme_classic(base_size = 14))

# Set a seed so that our train/test sets stay the same
set.seed(1234)

## Load the dataset ####
df <- read.csv("./Datasets/penguins_size.csv")

# Some methods require the outcome groups to be factors
df$species <- as.factor(df$species)

### Define colnames ####
# Lets define the swedish and english column names that are used for our plots
eng_colnames <- c("Species", "Culmenlength", "Culmendepth", "Flipperlength", "Weight")
swe_colnames <- c("Art", "Culmenlängd", "Culmendjup", "Vinglängd", "Vikt")

# For this analysis we will exclude the "island"  and "sex" variable.
df <- subset(df, select = -c(island, sex))

# Change the colnames to swedish
colnames(df) <- swe_colnames

## Missing values ####

# We find that we have 8 missing values in our dataset
sum(is.na(df))

# We also find that the missing values are all contained in two rows
df[is.na(df$Culmenlängd), ]

# For this reason we find that imputation is not a good choice and would rather
# just remove the two observations.
df <- df[!is.na(df$Culmenlängd), ]

# Lets create the train and test data split
perc_train <- 0.7
split <- sample(c(rep(0, perc_train * nrow(df)), rep(1, (1-perc_train) * nrow(df))))
train <- df[split == 0, ]   
test <- df[split == 1, ]  

# Summary statistics ####

# The standard deviations of our variables are
# Function that calculates our summary stats
summaryStats <- function(species, df) {
  if (species == "All") {
    means <- sapply(df[2:5], mean)
    sds <- sapply(df[2:5], sd)
  } else {
    means <- sapply(df[df$Art == species, ][2:5], mean)
    sds <- sapply(df[df$Art == species, ][2:5], sd)
  }
  result <- paste(round(means, 1), " (", round(sds, 1), ")", sep = "")
  return(as.vector(result))
}

## Summary stats for the dataset ####
summary_stats <- rbind(summaryStats("Gentoo", df),
                       summaryStats("Chinstrap", df),
                       summaryStats("Adelie", df),
                       summaryStats("All", df))

rownames(summary_stats) <- c("Gentoo", "Chinstrap", "Adelie", "All species")
colnames(summary_stats) <- eng_colnames[2:5]
summary_stats

# Tests ####

## Normality tests ####
# Mardia's test
mvn(subset(df, select = -Art), mvnTest = "mardia")$multivariateNormality
# Henze-Zirkler's test
mvn(subset(df, select = -Art), mvnTest = "hz")$multivariateNormality
# Royston's test
mvn(subset(df, select = -Art), mvnTest = "royston")$multivariateNormality

## Equal covariances test ####
# We will have to look at the covariance matrices manually since
# the test for equal covariances require the data to be multivariate
# normal.
round(cov(df[df$Art == "Gentoo", ][, 2:5]), 1)
round(cov(df[df$Art == "Chinstrap", ][, 2:5]), 1)
round(cov(df[df$Art == "Adelie", ][, 2:5]), 1)

# Model training ####

# The model is defined as 
lda <- lda(Art ~ ., data = train)


## Prediction outputs ####
lda_pred_train <- predict(lda)
lda_pred_test <- predict(lda, newdata = test)

# Lets create two dataframes with useful data to plot
lda_output_train <- data.frame(
  'LD1' = lda_pred_train$x[, 1],
  'LD2' = lda_pred_train$x[, 2],
  "Art" = lda_pred_train$class)

lda_output_test <- data.frame(
  'LD1' = lda_pred_test$x[, 1],
  'LD2' = lda_pred_test$x[, 2],
  "Art" = lda_pred_test$class)

# Plots ####
## Swedish ####
colnames(df) <- swe_colnames

### Frequency plot ####
ggplot(data = df, aes(x = Art, fill = Art)) +
  geom_bar() +
  ylab("Antal") +
  labs(title=paste("Antal pingviner (n=", nrow(df), ")", sep="")) +
  geom_text(aes(label=paste(..count.., " (", round(..count../nrow(df)*100, 1), "%)", sep="")), 
            stat="count", nudge_y = 4) +
  theme(legend.position = "none")
ggsave(filename = "freq_plot.png", path = "./Plots", 
       height = 5, width = 5)

### Pairplot ####
ggpairs(df, columns = 2:5, aes(color = Art, alpha = 0.05)) +
  labs(title = "Sammanfattning av våra oberoende variabler")
ggsave(filename = "var_summary.png", path = "./Plots", 
       height = 9, width = 9)

### Dim reduction on train data ####
lda_plot <- ggplot(lda_output_train, aes(x = LD1, y = LD2, color=Art)) +
  geom_point(size = 4, alpha=0.6) + 
  labs(title = "De två diskriminantfunktionerna applicerade på träningsdatat") +
  theme(legend.position = "bottom") 
ggMarginal(lda_plot, type="histogram", groupFill = TRUE, 
           xparams = list(bins=c(25)),
           yparams = list(bins=c(25)))

### Dim reduction on test data ####
lda_plot <- ggplot(lda_output_test, aes(x = LD1, y = LD2, color=Art)) +
  geom_point(size = 4, alpha=0.6) + 
  labs(title = "De två diskriminantfunktionerna applicerade på testdatat") +
  theme(legend.position = "bottom") 
ggMarginal(lda_plot, type="histogram", groupFill = TRUE, 
           xparams = list(bins=c(25)),
           yparams = list(bins=c(25)))

### Partition plot ####
partimat(Art ~ Culmenlängd + Vinglängd, data = df, method = "lda")

## English ####
colnames(df) <- eng_colnames
colnames(lda_output_train)[3] <- "Species"
colnames(lda_output_test)[3] <- "Species"

### Frequency plot ####
ggplot(data = df, aes(x = Species, fill = Species)) +
  geom_bar() +
  ylab("Count") +
  labs(title=paste("Number of penguins (n=", nrow(df), ")", sep="")) +
  geom_text(aes(label=paste(..count.., " (", round(..count../nrow(df)*100, 1), "%)", sep="")), 
            stat="count", nudge_y = 4) +
  theme(legend.position = "none")
ggsave(filename = "freq_plot_eng.png", path = "./Plots", 
       height = 5, width = 5)

### Pairplot ####
ggpairs(df, columns = 2:5, aes(color = Species, alpha = 0.05)) +
  labs(title = "Summary of our independent variables")
ggsave(filename = "var_summary_eng.png", path = "./Plots", 
       height = 9, width = 9)

### Dim reduction on train data ####
lda_plot <- ggplot(lda_output_train, aes(x = LD1, y = LD2, color=Species)) +
  geom_point(size = 4, alpha=0.6) + 
  labs(title = "The two discriminant functions applied on the train data") +
  theme(legend.position = "bottom") 
ggMarginal(lda_plot, type="histogram", groupFill = TRUE, 
           xparams = list(bins=c(25)),
           yparams = list(bins=c(25)))

### Dim reduction on test data ####
lda_plot <- ggplot(lda_output_test, aes(x = LD1, y = LD2, color=Species)) +
  geom_point(size = 4, alpha=0.6) + 
  labs(title = "The two discriminant functions applied on the test data") +
  theme(legend.position = "bottom") 
ggMarginal(lda_plot, type="histogram", groupFill = TRUE, 
           xparams = list(bins=c(25)),
           yparams = list(bins=c(25)))

### Partition plot ####
partimat(Species ~ Culmenlength + Flipperlength, data = df, method = "lda")

# Model evaluation ####

# We get a summary of our evaluation metrics using
eval_metrics <- confusionMatrix(table(lda_output_test$Art, test$Art))
eval_metrics$table # Confusion matrix
round(eval_metrics$overall["Accuracy"]*100, 1) # Hitrate

# The maximum criterion would give us a hit rate of
max(round(prop.table(table(test$Art))*100, 1)) 

# The hit rate using proportional chance criterion would b
round(sum(prop.table(table(test$Art))^2)*100, 1)
