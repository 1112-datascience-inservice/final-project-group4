library(ggplot2)
library(dplyr)
library(magrittr)
library(outliers)


train <- read.table("data/train.csv", sep = ",", header = TRUE)
test <- read.table("data/test.csv", sep = ",", header = TRUE)
train_id <- train$Id
test_id <- test$Id

train <- train[, -1]
test <- test[, -1]

summary(train)
outlier(train$GrLivArea)

col_types <- sapply(train, class)
for (i in which(col_types == "integer")) {
   print(grubbs.test(train[, i])[3] < 0.05)
}