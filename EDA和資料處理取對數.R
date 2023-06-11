args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("[USAGE] Rscript finalproject_EDA.R --train data/train.csv", call.=FALSE)
}
# get input, output path, and fold
f_in_train <- NA


# parse input, output, and fold
for (i in seq_along(args)) {
  if (args[i] == '--train') {
    f_in_train <- args[i+1]
  } 
}
# check whether there is input, output, or fold
if (is.na(f_in_train)) {
  stop("Unable to identify input file, please use --train Data/train.csv", call.=FALSE)
}
if (!file.exists(f_in_train)) {
  stop((sprintf("%s doesn't exist", f_in_train)), call.=FALSE)
}

message("input train file = ", f_in_train)




# 引用套件
library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(scales)
library(gplots)


# 讀取csv檔案
train <- read.table("/Users/Jie/Desktop/final-project-group4/data/train.csv", sep = ",", header = TRUE)

# 觀察離群值
ggplot(train, aes(x = GrLivArea, y = SalePrice)) +
  geom_point() +
  geom_text(aes(label = Id), vjust = -1, hjust = 1, size = 3) +
  labs(y = "SalePrice", x = "GrLivArea") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)


# 計算平均數和標準差
mean_value <- mean(train$SalePrice)
sd_value <- sd(train$SalePrice)

# Plot the distribution
ggplot(train, aes(x = SalePrice)) +
  geom_density() +
  geom_histogram(aes(y = ..density..), alpha = 0.5, fill = "lightblue") +
  stat_function(fun = dnorm, args = list(mean = mean_value, sd = sd_value), color = "red") +
  geom_text(x = mean_value, y = 0, label = paste("Mean:", round(mean_value, 2)), vjust = 1, hjust = 0, color = "blue") +
  geom_text(x = mean_value, y = 0, label = paste("SD:", round(sd_value, 2)), vjust = -1, hjust = 0, color = "blue") +
  labs(x = "SalePrice", y = "Frequency") +
  ggtitle("SalePrice Distribution") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

# Get the fitted parameters
fit <- fitdist(as.vector(train$SalePrice), "norm")
mu <- fit$estimate["mean"]
sigma <- fit$estimate["sd"]
print(paste("mu =", round(mu, 2), "and sigma =", round(sigma, 2)))

# Plot the QQ-plot
qqnorm(train$SalePrice, main = "quantile-quantile plot")
  scale_y_continuous(labels = scales::comma)
qqline(train$SalePrice)

# 數值型特徵相關性矩陣圖
train_data <- train[,2:81]
numeric_train <- train_data[sapply(train_data, is.numeric)]
numeric_train[is.na(numeric_train)] <- 0
# 計算相關係數
corrmat <- cor(numeric_train)

# 設定顏色映射
colors <- colorRampPalette(c("blue", "white", "red"))(100)

# 繪製特徵熱圖並標示相關程度數值
heatmap.2(corrmat, col = colors, main = "Correlation Heatmap", scale = "none",
          trace = "none", key = TRUE, key.title = "Correlation", key.xlab = NULL, key.ylab = NULL,
          keysize = 1, density.info = "none", cexRow = 0.8, cexCol = 0.8,
          margins = c(10, 10), srtCol = 90, adjCol = c(0.8, 0.5))
# 調整圖表大小
par(mar = c(8, 4, 4, 8) + 0.1)

# 做對數變換，讓資料接近正態分佈
train$SalePrice <- log1p(train$SalePrice)

# 檢查新的分佈
ggplot(train, aes(x = SalePrice)) +
  geom_density() +
  geom_histogram(aes(y = ..density..), alpha = 0.5, fill = "lightblue") +
  stat_function(fun = dnorm
                , args = list(mean = mean(train$SalePrice)
                              , sd = sd(train$SalePrice)), color = "red") +
  labs(x = "SalePrice", y = "Density") +
  ggtitle("New SalePrice Distribution")

# 取得函數使用的擬合參數
fit <- fitdist(train$SalePrice, "norm")
mu <- fit$estimate["mean"]
sigma <- fit$estimate["sd"]
cat(paste("mu =", round(mu, 2), "and sigma =", round(sigma, 2), "\n"))

# 繪製分佈圖
qqnorm(train$SalePrice, main = "New quantile-quantile plot")
qqline(train$SalePrice)
grid()