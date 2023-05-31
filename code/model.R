library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(magrittr)
library(outliers)


train <- read.table("data/train.csv", sep = ",", header = TRUE)
test <- read.table("data/test.csv", sep = ",", header = TRUE)
train_id <- train$Id
test_id <- test$Id

train <- train[, -1]
test <- test[, -1]



# summary(train)
# outlier(train$GrLivArea)

# col_types <- sapply(train, class)
# for (i in which(col_types == "integer")) {
#    print(grubbs.test(train[, i])[3] < 0.05)
# }

# Plot the distribution
ggplot(train, aes(x = SalePrice)) +
  geom_density() +
  geom_histogram(aes(y = ..density..), alpha = 0.5, fill = "lightblue") +
  stat_function(fun = dnorm, args = list(mean = mean(train$SalePrice), sd = sd(train$SalePrice)), color = "red") +
  labs(x = "SalePrice", y = "Density") +
  ggtitle("SalePrice Distribution")

# Get the fitted parameters
fit <- fitdist(train$SalePrice, "norm")
mu <- fit$estimate["mean"]
sigma <- fit$estimate["sd"]
print(paste("mu =", round(mu, 2), "and sigma =", round(sigma, 2)))

# Plot the QQ-plot
qqnorm(train$SalePrice, main = "QQ-plot")
qqline(train$SalePrice)
grid()


# 做對數變換，讓資料接近正態分佈
train$SalePrice <- log1p(train$SalePrice)

# 檢查新的分佈
ggplot(train, aes(x = SalePrice)) +
  geom_density() +
  geom_histogram(aes(y = ..density..), alpha = 0.5, fill = "lightblue") +
  stat_function(fun = dnorm, args = list(mean = mean(train$SalePrice), sd = sd(train$SalePrice)), color = "red") +
  labs(x = "SalePrice", y = "Density") +
  ggtitle("SalePrice Distribution")

# 取得函數使用的擬合參數
fit <- fitdist(train$SalePrice, "norm")
mu <- fit$estimate["mean"]
sigma <- fit$estimate["sd"]
cat(paste("mu =", round(mu, 2), "and sigma =", round(sigma, 2), "\n"))

# 繪製分佈圖
qqnorm(train$SalePrice, main = "QQ-plot")
qqline(train$SalePrice)
grid()

ntrain <- nrow(train)
ntest <- nrow(test)
y_train <- train$SalePrice
all_data <- bind_rows(train, test)  # 合併資料集
all_data <- all_data[!names(all_data) == "SalePrice"]  # 刪除 SalePrice 欄位
cat(paste("all_data size is :", dim(all_data), "\n"))

all_data_na <- (colSums(is.na(all_data)) / nrow(all_data)) * 100
all_data_na <- all_data_na[all_data_na > 0]
all_data_na <- sort(all_data_na, decreasing = TRUE)[1:30]
missing_data <- data.frame('Missing Ratio' = all_data_na)
head(missing_data, 20)

par(mfrow = c(1, 1))
barplot(all_data_na, horiz = TRUE, cex.names = 0.8, las = 2, main = "Percent missing data by feature",
        xlab = "Percent of missing values", ylab = "Features")
axis(side = 2, at = 1:length(all_data_na), labels = all_data_na.index, las = 1)
box()
title(main = "Percent missing data by feature", xlab = "Percent of missing values", ylab = "Features", cex.main = 1, cex.lab = 1)

