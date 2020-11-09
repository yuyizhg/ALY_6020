setwd('/Users/yuyizhang/FCR/NEU/CPS/Analytics_2018/ALY 6020_Predictive Analytics/Week 6 Final Project')
library(ggplot2)
library(cowplot)
data <- read.csv("heart disease.csv")

# Data Preparation
head(data)

colnames(data) <- c(
  "age",
  "sex",# 0 = female, 1 = male
  "cp", # chest pain
  # 1 = typical angina,
  # 2 = atypical angina,
  # 3 = non-anginal pain,
  # 4 = asymptomatic
  "trestbps", # resting blood pressure (in mm Hg)
  "chol", # serum cholestoral in mg/dl
  "fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
  "restecg", # resting electrocardiographic results
  # 1 = normal
  # 2 = having ST-T wave abnormality
  # 3 = showing probable or definite left ventricular hypertrophy
  "thalach", # maximum heart rate achieved
  "exang",   # exercise induced angina, 1 = yes, 0 = no
  "oldpeak", # ST depression induced by exercise relative to rest
  "slope", # the slope of the peak exercise ST segment
  # 1 = upsloping
  # 2 = flat
  # 3 = downsloping
  "ca", # number of major vessels (0-3) colored by fluoroscopy
  "thal", # this is short of thalium heart scan
  # 3 = normal (no cold spots)
  # 6 = fixed defect (cold spots during rest and exercise)
  # 7 = reversible defect (when cold spots only appear during exercise)
  "hd" # (the predicted attribute) - diagnosis of heart disease
  # 0 if less than or equal to 50% diameter narrowing
  # 1 if greater than 50% diameter narrowing
)
head(data)

str(data)

## First, convert "?"s to NAs...
data[data == "?"] <- NA
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$ca <- as.integer(data$ca) # since this column had "?"s in it
# R thinks that the levels for the factor are strings, but
# we know they are integers, so first convert the strings to integiers...
data$ca <- as.factor(data$ca)  # ...then convert the integers to factor levels
data$thal <- as.integer(data$thal) # "thal" also had "?"s in it.
data$thal <- as.factor(data$thal)

## This next line replaces 0 and 1 with "Healthy" and "Unhealthy"
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd) # Now convert to a factor
str(data)

## Now determine how many rows have "NA" (aka "Missing data"). If it's just
## a few, we can remove them from the dataset, otherwise we should consider
## imputing the values with a Random Forest or some other imputation method.
nrow(data[is.na(data$ca) | is.na(data$thal),])
data[is.na(data$ca) | is.na(data$thal),]
nrow(data)
data <- data[!(is.na(data$ca) | is.na(data$thal)),]
nrow(data)


## Now we can do some quality control by making sure all of the factor
## levels are represented by people with and without heart disease (hd)
xtabs(~ hd + sex, data=data)
xtabs(~ hd + cp, data=data)
xtabs(~ hd + fbs, data=data)
xtabs(~ hd + restecg, data=data)
xtabs(~ hd + exang, data=data)
xtabs(~ hd + slope, data=data)
xtabs(~ hd + ca, data=data)
xtabs(~ hd + thal, data=data)






# Decision Trees
str(data) 
table(data$hd)
## Data preparation - creating random training and test datasets
set.seed(123)
train_sample <- sample(1000, 900)
data_train <- data[train_sample, ]
data_test <- data[-train_sample, ]
prop.table(table(data_train$hd))
prop.table(table(data_test$hd))

## Training a model on the data
library(C50)
data_model <- C5.0(data_train[-14], data_train$hd)
data_model
summary(data_model)

## Evaluating model performance
library(gmodels)
data_pred <- predict(data_model, data_test)
CrossTable(data_test$hd, data_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))

## improving model performance
### Boosting the accuracy of decision trees
data_boost10 <- C5.0(data_train[-14], data_train$hd,
                       trials = 10)
data_boost10
summary(data_boost10)

data_boost10_pred10 <- predict(data_boost10, data_test)
CrossTable(data_test$hd, data_boost10_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))





# Random Forests
## Training a model on the data
library(randomForest)
library(MASS)
set.seed(17)
data.rf <- randomForest(hd ~ ., data = data,
                        mtry = 4, importance = TRUE,
                        do.trace = 100)

## evaluating model performance
print(data.rf)

## improving model performance
# Model Comparison with Errorest Functions
library(ipred)
set.seed(131)
error.RF <- numeric(10)
for (i in 1:10) error.RF[i] <-
  errorest(hd ~ ., data = data,
           model = randomForest, mtry = 4)$error
summary(error.RF)  

library(e1071)
set.seed(563)
error.SVM <- numeric(10)
for (i in 1:10) error.SVM[i] <-
  errorest(hd ~ ., data = data,
           model = svm, cost = 10, gamma = 1.5)$error
summary(error.SVM)

par(mfrow = c(2, 2))
for (i in 1:4) {
  plot(sort(data.rf$importance[,i], dec = TRUE),
       type = "h", main = paste("Measure", i))
}





# Logistic Regression
logistic <- glm(hd ~ ., data=data, family="binomial")
summary(logistic) 
## Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null
## The p-value for the R^2
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))
## now we can plot the data
predicted.data <- data.frame(
  probability.of.hd=logistic$fitted.values,
  hd=data$hd)
predicted.data <- predicted.data[
  order(predicted.data$probability.of.hd, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)
## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
ggplot(data=predicted.data, aes(x=rank, y=probability.of.hd)) +
  geom_point(aes(color=hd), alpha=1, shape=4, stroke=2) + 
  xlab("Index") +
  ylab("Predicted probability of getting heart disease")

