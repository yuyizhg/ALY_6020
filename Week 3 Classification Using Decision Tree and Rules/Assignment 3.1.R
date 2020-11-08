# Step 1 – collecting data
setwd('/Users/yuyizhang/FCR/NEU/CPS/Analytics_2018/ALY 6020_Predictive Analytics/Week 3 Classification Using Decision Tree and Rules')


# Step 2 – exploring and preparing the data
credit <- read.csv('credit.csv')
str(credit)

table(credit$Status.of.existing.checking.account)
table(credit$Savings.account.bonds)

summary(credit$Duration.in.month)
summary(credit$Credit.amount)

credit$Default <- factor(credit$Default, levels = c("1", "2"), labels = c("no", "yes"))
table(credit$Default)

## Data preparation – creating random training and test datasets
set.seed(123)
train_sample <- sample(1000, 900)
str(train_sample)

credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]
prop.table(table(credit_train$Default))
prop.table((table(credit_test$Default)))


# Step 3 – training a model on the data
library(C50)
credit_model <- C5.0(credit_train[-21], credit_train$Default)
credit_model
summary(credit_model)


# Step 4 – evaluating model performance
credit_pred <- predict(credit_model, credit_test)
library(gmodels)
CrossTable(credit_test$Default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


# Step 5 – improving model performance
## Boosting the accuracy of decision trees
credit_boost10 <- C5.0(credit_train[-21], credit_train$Default,
                       trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost10_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$Default, credit_boost10_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

## Making mistakes more costlier than others
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions

error_cost <- matrix(c(0, 1, 4, 0), nrow = 2,
                     dimnames = matrix_dimensions)
error_cost

credit_cost <- C5.0(credit_train[-21], credit_train$Default,
                    costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$Default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

