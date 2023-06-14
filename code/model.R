library(ggplot2)
library(dplyr)
library(magrittr)
library(MASS)
library(caret)
library(janitor)
library(fitdistrplus)

library(randomForest)
library(lightgbm)
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
xgb_kfold <- function(data, k, target_col) {
  set.seed(123)
  indices <- sample(1:k, nrow(data), replace = TRUE)
  folds <- lapply(1:k, function(i) data[indices == i, ])

  models <- vector("list", k)
  predictions <- vector("list", k)
  ground_truth <- vector("list", k)
  rmse <- vector("numeric", k)

  for (i in 1:k) {
    test <- folds[[i]]
    train <- do.call(rbind, folds[-i])
    x_train <- train[, !(names(train) %in% target_col)]
    y_train <- train[[target_col]]
    x_test <- test[, !(names(test) %in% target_col)]
    y_test <- test[[target_col]]

    # Hyperparameter tuning
    best_rmse <- Inf
    best_params <- NULL
    params_grid <- list(
      nrounds = seq(100, 300, 50),
      max_depth = seq(3, 11, 2),
      eta = c(0.01, 0.05, 0.1)
    )

    for (nrounds in params_grid$nrounds) {
      for (max_depth in params_grid$max_depth) {
        for (eta in params_grid$eta) {
          xgb_model <- xgboost(
            data = as.matrix(x_train),
            label = y_train,
            nrounds = nrounds,
            max_depth = max_depth,
            eta = eta,
            verbose = 0
          )
          y_pred <- predict(xgb_model, as.matrix(x_test))
          current_rmse <- sqrt(mean((y_test - y_pred) ^ 2))

          if (current_rmse < best_rmse) {
            best_rmse <- current_rmse
            best_params <- list(
              nrounds = nrounds
              , max_depth = max_depth
              , eta = eta
            )
          }
        }
      }
    }

    # Train model with best parameters
    best_xgb_model <- xgboost(
      data = as.matrix(x_train),
      label = y_train,
      nrounds = best_params$nrounds,
      max_depth = best_params$max_depth,
      eta = best_params$eta,
      verbose = 0
    )

    y_pred <- predict(best_xgb_model, as.matrix(x_test))
    rmse[i] <- sqrt(mean((y_test - y_pred) ^ 2))

    models[[i]] <- best_xgb_model
    predictions[[i]] <- y_pred
    ground_truth[[i]] <- y_test
  }

  avg_rmse <- mean(rmse)
  return(list(
    models = models,
    predictions = predictions,
    ground_truth = ground_truth,
    rmse = avg_rmse
  ))
}

glm_kfold <- function(data, k, target_col, family = gaussian()) {
  set.seed(123)
  indices <- sample(1:k, nrow(data), replace = TRUE)
  folds <- lapply(1:k, function(i) data[indices == i, ])

  models <- vector("list", k)
  predictions <- vector("list", k)
  ground_truth <- vector("list", k)
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
    ground_truth[[i]] <- y_test
  }
  avg_rmse <- mean(rmse)
  return(list(
    models = models
    , predictions = predictions
    , ground_truth = ground_truth
    , rmse = avg_rmse
    )
  )
}


rf_kfold <- function(data, k, target_col, ntree = 500) {
  set.seed(123)
  indices <- sample(1:k, nrow(data), replace = TRUE)
  folds <- lapply(1:k, function(i) data[indices == i, ])

  models <- vector("list", k)
  predictions <- vector("list", k)
  ground_truth <- vector("list", k)
  rmse <- vector("numeric", k)

  for (i in 1:k) {
    # 分割資料為訓練集和測試集
    test_data <- folds[[i]]
    train_data <- do.call(rbind, folds[-i])

    # 分割特徵和目標變數
    x_train <- train_data[, !(names(train_data) %in% target_col)]
    y_train <- train_data[[target_col]]
    x_test <- test_data[, !(names(test_data) %in% target_col)]
    y_test <- test_data[[target_col]]

    # 超參數調整
    best_rmse <- Inf
    best_params <- NULL
    params_grid <- list(
      mtry = seq(10, 90, 20),
      nodesize = seq(10, 50, 20)
    )

    for (mtry in params_grid$mtry) {
      for (nodesize in params_grid$nodesize) {
        rf_model <- randomForest(
          x = x_train,
          y = y_train,
          ntree = ntree,
          mtry = mtry,
          nodesize = nodesize
        )
        y_pred <- predict(rf_model, newdata = x_test)
        current_rmse <- sqrt(mean((y_test - y_pred) ^ 2))

        if (current_rmse < best_rmse) {
          best_rmse <- current_rmse
          best_params <- list(mtry = mtry, nodesize = nodesize)
        }
      }
    }

    # 使用最佳參數重新訓練模型
    best_rf_model <- randomForest(
      x = x_train,
      y = y_train,
      ntree = ntree,
      mtry = best_params$mtry,
      nodesize = best_params$nodesize
    )

    # 使用最佳模型進行預測
    y_pred <- predict(best_rf_model, newdata = x_test)
    rmse[i] <- sqrt(mean((y_test - y_pred) ^ 2))

    models[[i]] <- best_rf_model
    predictions[[i]] <- y_pred
    ground_truth[[i]] <- y_test
  }

  avg_rmse <- mean(rmse)
  return(list(
    models = models,
    predictions = predictions,
    ground_truth = ground_truth,
    rmse = avg_rmse
  ))
}



xgb_outcome <- xgb_kfold(train_data, 5, "SalePrice")
glm_outcome <- glm_kfold(train_data, 5, "SalePrice")
rf_outcome <- rf_kfold(train_data, 5, "SalePrice")


meta_data <- data.frame(
  xgb_pre = unlist(xgb_outcome$predictions)
  , glm_pre = unlist(glm_outcome$predictions)
  , rf_pre = unlist(rf_outcome$predictions)
  , y = unlist(xgb_outcome$ground_truth)
)


#grid search
#create hyperparameter grid
hyper_grid <- expand.grid(
  max_depth = seq(3, 9, 2)
  , num_leaves = seq(10, 30, 10)
  , num_iterations = seq(20, 100, 20)
  , learning_rate = seq(.3, .5, .1)
)
hyper_grid <- unique(hyper_grid)


lightgbm_hyperparameter_tuning <- function(data, target_col, hyper_grid) {
  set.seed(123)
  num_models <- nrow(hyper_grid)
  models <- vector("list", num_models)
  rmse_scores <- vector("numeric", num_models)

  for (i in 1:num_models) {
    params <- hyper_grid[i, ]
    max_depth <- as.integer(params[["max_depth"]])
    num_leaves <- as.integer(params[["num_leaves"]])
    num_iterations <- as.integer(params[["num_iterations"]])
    learning_rate <- params[["learning_rate"]]

    lgb_data <- lgb.Dataset(
      data = as.matrix(data[, !(names(data) %in% target_col)])
      , label = data[[target_col]]
    )

    model <- lgb.train(
      data = lgb_data,
      params = list(
        objective = "regression",
        metric = "rmse",
        max_depth = max_depth,
        num_leaves = num_leaves,
        num_iterations = num_iterations,
        learning_rate = learning_rate
      )
    )

    models[[i]] <- model
    model_pre <- predict(
      model
      , as.matrix(data[, !(names(data) %in% target_col)])
    )
    rmse_scores[i] <- sqrt(mean((data[[target_col]] - model_pre) ^ 2))
  }

  best_model_index <- which.min(rmse_scores)
  best_model <- models[[best_model_index]]
  best_params <- hyper_grid[best_model_index, ]
  best_rmse <- rmse_scores[best_model_index ]

  return(list(
    best_model = best_model,
    best_params = best_params,
    rmse_scores = best_rmse
  ))
}


lightgbm_best <- lightgbm_hyperparameter_tuning(
  data = meta_data
  , target_col = "y"
  , hyper_grid = hyper_grid
)

meta_data_test <- data.frame(
  xgb_pre = xgb_predict(xgb_outcome$models, test_data)
  , glm_pre = glm_predict(glm_outcome$models, test_data)
  , rf_pre = glm_predict(glm_outcome$models, test_data)
)


xgb_predict <- function(models, test_data) {
  num_models <- length(models)
  predictions <- vector("list", num_models)

  for (i in 1:num_models) {
    model <- models[[i]]
    predictions[[i]] <- predict(model, as.matrix(test_data))
  }

  avg_predictions <- rowMeans(do.call(cbind, predictions))
  return(avg_predictions)
}
glm_predict <- function(models, test_data) {
  num_models <- length(models)
  predictions <- vector("list", num_models)

  for (i in 1:num_models) {
    model <- models[[i]]
    predictions[[i]] <- predict(model, test_data)
  }

  avg_predictions <- rowMeans(do.call(cbind, predictions))
  return(avg_predictions)
}


meta_data_test <- data.frame(
  xgb_pre = xgb_predict(xgb_outcome$models, test_data)
  , glm_pre = glm_predict(glm_outcome$models, test_data)
  , rf_pre = glm_predict(glm_outcome$models, test_data)
)



lgb_test_pre <- predict(lightgbm_best$best_model, as.matrix(meta_data_test))


output <- data.frame(
  ID = test_id
  , SalePrice = expm1(lgb_test_pre)
)

write.table(output, file = "./result/output.csv"
    , sep = ","
    , col.names = TRUE
    , quote = FALSE
    , row.names = FALSE
)


weight_sum <- sum(
  1 / xgb_outcome$rmse
  , 1 / glm_outcome$rmse
  , 1 / rf_outcome$rmse
)

meta_data_test$xgb_pre <- meta_data_test$xgb_pre * 1 / xgb_outcome$rmse / weight_sum
meta_data_test$glm_pre <- meta_data_test$glm_pre * 1 / glm_outcome$rmse / weight_sum
meta_data_test$rf_pre <- meta_data_test$rf_pre * 1 / rf_outcome$rmse / weight_sum

pre <- apply(meta_data_test, 1, sum)

output_2 <- data.frame(
  ID = test_id
  , SalePrice = expm1(pre)
)

write.table(output_2, file = "./result/output_2.csv"
    , sep = ","
    , col.names = TRUE
    , quote = FALSE
    , row.names = FALSE
)


rmse_table <- data.frame(
  xgb_train_rmse = xgb_outcome$rmse
  , glm_train_rmse = glm_outcome$rmse
  , rf_train_rmse = rf_outcome$rmse
  , lgb_train_rmse = lightgbm_best$rmse_scores
)


