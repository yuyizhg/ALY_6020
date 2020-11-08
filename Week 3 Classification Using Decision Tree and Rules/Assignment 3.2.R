# Step 1 – collecting data
setwd('/Users/yuyizhang/FCR/NEU/CPS/Analytics_2018/ALY 6020_Predictive Analytics/Week 3 Classification Using Decision Tree and Rules')


# Step 2 – exploring and preparing the data
mushroom <- read.csv('mushroom.csv')
str(mushroom)

mushroom$veil.type <- NULL
mushroom$type <- factor(mushroom$type, levels = c("e", "p"), labels = c("edible", "poisonous"))
table(mushroom$type)


# Step 3 – training a model on the data
library(RWeka)
mushroom <- lapply(mushroom, as.factor)
mushroom_1R <- OneR(type ~ ., data = mushroom)
mushroom_1R

mushroom$odor <- factor(mushroom$odor, 
                        levels = c("a", "l", "c", "y", "f", "m", "n", "p", "s"),
                        labels = c("almond", "anise", "creosote", "fishy", "foul", "musty", "none", "pungent", "spicy"))
mushroom_1R <- OneR(type ~ ., data = mushroom)
mushroom_1R


# Step 4 – evaluating model performance
summary(mushroom_1R)


# Step 5 – improving model performance
mushroom_JRip <- JRip(type ~ ., data = mushroom)
mushroom_JRip
summary(mushroom_JRip)
