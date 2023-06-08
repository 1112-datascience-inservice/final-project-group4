library(ggplot2)
library(dplyr)
library(magrittr)
library(MASS)
library(caret)
library(janitor)
library(xgboost)


train <- read.table("data/train.csv", sep = ",", header = TRUE)
test <- read.table("data/test.csv", sep = ",", header = TRUE)
train_id <- train$Id
test_id <- test$Id

train <- train[, -1]
test <- test[, -1]



# Plot the distribution
ggplot(train, aes(x = SalePrice)) +
  geom_density() +
  geom_histogram(aes(y = ..density..), alpha = 0.5, fill = "lightblue") +
  stat_function(fun = dnorm
    , args = list(mean = mean(train$SalePrice)
    , sd = sd(train$SalePrice)), color = "red") +
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
  stat_function(fun = dnorm
    , args = list(mean = mean(train$SalePrice)
    , sd = sd(train$SalePrice)), color = "red") +
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

# 遺失值處理
ntrain <- nrow(train)
ntest <- nrow(test)
y_train <- train$SalePrice
all_data <- bind_rows(train, test)  # 合併資料集
all_data <- all_data[!names(all_data) == "SalePrice"]  # 刪除 SalePrice 欄位

all_data_na <- (colSums(is.na(all_data)) / nrow(all_data)) * 100
all_data_na <- all_data_na[all_data_na > 0]
all_data_na <- sort(all_data_na, decreasing = TRUE)[1:30]
missing_data <- data.frame("Missing Ratio" = all_data_na)
head(missing_data, 20)

par(mfrow = c(1, 1))
barplot(all_data_na, horiz = TRUE
  , cex.names = 0.8, las = 2
  , main = "Percent missing data by feature"
  , xlab = "Percent of missing values", ylab = "Features")
box()

# 用 "None" 或 0 填充缺失值
none_col <- c("PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu"
  , "MasVnrType", "GarageType", "GarageFinish", "GarageQual", "GarageCond"
  , "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2"
)

all_data[, none_col] <- lapply(all_data[, none_col]
  , function(x) ifelse(is.na(x), "None", x))

zero_col <- c("MasVnrArea", "GarageYrBlt", "GarageArea"
  , "GarageCars", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF"
  , "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath"
)

all_data[, zero_col] <- lapply(all_data[, zero_col]
  , function(x) ifelse(is.na(x), 0, x))

# 缺失較少的類別型特徵，用眾數填補缺失值

mode_col <- c("MSZoning", "Electrical", "KitchenQual"
  , "Exterior1st", "Exterior2nd", "SaleType")

mode_naomit <- function(x) {
  ux <- na.omit(unique(x))
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

all_data[, mode_col] <- lapply(all_data[, mode_col]
  , function(x) ifelse(is.na(x), mode_naomit(x), x))


# 以各個房屋所在鄰域（Neighborhood）的LotFrontage中位數作為填補值

all_data$LotFrontage <- ave(all_data$LotFrontage, all_data$Neighborhood
  , FUN = function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# 用 "Typ" 填補 "Functional" 欄位的缺失值
all_data$Functional <- ifelse(is.na(all_data$Functional)
  , "Typ", all_data$Functional)

# 刪除 "Utilities" 欄位，因其方差非常小
all_data <- subset(all_data, select = -c(Utilities))

# 最後確認是否已處理完所有缺失值
sum(is.na(all_data))

# 將數值型特徵轉換為類別型特徵
all_data$MSSubClass <- as.character(all_data$MSSubClass)
all_data$YrSold <- as.character(all_data$YrSold)
all_data$MoSold <- as.character(all_data$MoSold)

# 將特徵的類別映射為有大小的數字
ordinal_mapping <- list(
  "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "None" = 0,
  "Fin" = 3, "RFn" = 2, "Unf" = 1, "Av" = 3,
  "GLQ" = 6, "ALQ" = 5, "BLQ" = 4, "Rec" = 3, "LwQ" = 2, "Unf" = 1,
  "Typ" = 8, "Min1" = 7, "Min2" = 6, "Mod" = 5,
  "Maj1" = 4, "Maj2" = 3, "Sev" = 2, "Sal" = 1,
  "GdPrv" = 4, "MnPrv" = 3, "GdWo" = 2, "MnWw" = 1,
  "Gtl" = 3, "Mod" = 2, "Sev" = 1,
  "Reg" = 4, "IR1" = 3, "IR2" = 2, "IR3" = 1,
  "Y" = 3, "P" = 2, "N" = 1,
  "Pave" = 2, "Grvl" = 1
)


ordinal_cols <- c(
  "FireplaceQu", "GarageQual", "GarageCond", "GarageFinish", "BsmtQual"
  , "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2"
  , "ExterQual", "ExterCond", "HeatingQC", "PoolQC"
  , "KitchenQual", "Functional", "Fence", "LandSlope"
  , "LotShape", "PavedDrive", "Street", "Alley", "CentralAir"
)

for (col in ordinal_cols) {
  all_data[[col]] <- recode(all_data[[col]], !!!ordinal_mapping, .default = 0)
}

numeric_feats <- names(all_data)[sapply(all_data, is.numeric)]
character_feats <- names(all_data)[sapply(all_data, is.character)]
dummy_variables <- dummyVars("~.", data = all_data[, character_feats])

# 套用變數轉換規則
encoded_data <- as.data.frame(predict(dummy_variables, newdata = all_data))

# 合併原始數據與 One-Hot Encoding 結果
all_data_encoded <- cbind(all_data[c(numeric_feats)], encoded_data)

train_data <- all_data_encoded[train_id, ]
test_data <- all_data_encoded[test_id, ]

train_data$SalePrice <- y_train

# xgboost
xgboost_kfold <- function(data, k, target_col) {
  indices <- sample(1:k, nrow(data), replace = TRUE)
  folds <- lapply(1:k, function(i) data[indices == i, ])

  models <- vector("list", k)
  predictions <- vector("list", k)
  rmse <- vector("numeric", k)

  for (i in 1:k) {
    test <- folds[[i]]
    train <- do.call(rbind, folds[-i])
    x_train <- train[, !(names(train_data) %in% target_col)]
    y_train <- train[[target_col]]
    x_test <- test[, !(names(train_data) %in% target_col)]
    y_test <- test[[target_col]]

    xgb_model <- xgboost(
        data = as.matrix(x_train)
        , label = y_train
        , nrounds = 40
        , verbose = 0
    )
    y_pred <- predict(xgb_model, as.matrix(x_test))
    rmse[i] <- sqrt(mean((y_test - y_pred) ^ 2))

    models[[i]] <- xgb_model
    predictions[[i]] <- y_pred
  }
  avg_rmse <- mean(rmse)
  return(list(models = models, predictions = predictions, rmse = avg_rmse))
}

glm_kfold <- function(data, k, target_col, family = gaussian()) {
  indices <- sample(1:k, nrow(data), replace = TRUE)
  folds <- lapply(1:k, function(i) data[indices == i, ])

  models <- vector("list", k)
  predictions <- vector("list", k)
  rmse <- vector("numeric", k)

  for (i in 1:k) {
    # 分割資料為訓練集和測試集
    test_data <- folds[[i]]
    train_data <- do.call(rbind, folds[-i])

    # 分割特徵和目標變數
    x_test <- test_data[, !(names(test_data) %in% target_col)]
    y_test <- test_data[[target_col]]

    # 建立 glm 模型
    glm_model <- glm(
      formula = paste(target_col, "~ .")
      , data = train_data
      , family = family
    )

    # 使用模型進行預測
    y_pred <- predict(glm_model, newdata = x_test)

    # 計算RMSE
    rmse[i] <- sqrt(mean((y_test - y_pred) ^ 2))

    # 儲存模型和預測結果
    models[[i]] <- glm_model
    predictions[[i]] <- y_pred
  }

  avg_rmse <- mean(rmse)
  return(list(models = models, predictions = predictions, rmse = avg_rmse))
}

xgboost_outcome <- xgboost_kfold(train_data, 5, "SalePrice")
glm_outcome <- glm_kfold(train_data, 5, "SalePrice")





output <- data.frame(
  ID = test_id
  , SalePrice = expm1(predict(model_xg, as.matrix(test_data)))
)



write.table(output, file = "output.csv"
    , sep = ","
    , col.names = TRUE
    , quote = FALSE
    , row.names = FALSE
)
