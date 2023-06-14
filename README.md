# [Group4] Predict House Sales Prices

## Contributors
|組員|系級|學號|工作分配|
|----------------|----------------|---------------|----------------|
|孫詩傑|資科碩專二|110971006|程式碼建構、簡報製作|
|許芳耀|資科碩專二|110971025|程式碼、Readme撰寫| 
|李昂縣|資科碩專二|110971018|簡報製作|
|陳韻清|資科碩專二|110971027|shiny製作|

## Quick start

```R
Rscript code/your_script.R --input data/training --output results/performance.tsv
```


### docs
* Your presentation, 1112_DS-FP_groupID.ppt/pptx/pdf (i.e.,1112_DS-FP_group1.ppt), by **06.08**
* Any related document for the project
  * i.e., software user guide

### data

* Input
  * Source
  * Format
  * Size 
* Output

### code
#### 目標值分析
- SalePrice是本次預測的目標，透過畫出分布圖及QQ圖(Quantile Quantile Plot)，如果QQ圖上的分佈情形接近在一條直線附近，代表數據近似於常態分佈。
- 目前SalePrice的分布呈現正偏態，我們透過對數變換將其進行調整。

#### 缺失值處理
- 缺失欄位如圖
- 對於缺失較多之欄位依其屬性進行**None**或是**0**進行填充
- 部分缺失較少的離散型特徵，則以眾數填充。
- 另外針對欄位**LotFrontage**，由於每個Neighborhood的LotFrontage很可能是比較相近的，所以各個房子所在Neighborhood的LotFrontage中位數進行填充。
  
#### 特徵轉換
- **MSSubClass**、**YrSold**、**MoSold**轉為類別特徵
- 將多數序列型特徵重新編碼
- 最終將類別型特徵進行One-Hot-Encode

#### 建立模型
- 本次採用3基本模型(Base-Model)進行Stacking
  1. **Linear Regression**
  2. **Random Forest**
  3. **XGBoost**

- 透過k-fold方式進行cross validation並計算各模型在training階段的**RMSE**數值
- 元模型(Meta-Model)則採用LightGBM進行，透過元模型將基本模型的預測值進行結合。

- 最終採用兩方式進行Ensemble
  1. 透過LightGBM將基本模型的預測值進行結合
  2. 透過3基本模型在training階段的rmse，進行權重分配，計算最終testing預測值

### results

#### Trainging Stage

| xgb_train_rmse | glm_train_rmse | rf_train_rmse | lgb_train_rmse |
|----------------|----------------|---------------|----------------|
| 0.120658222    | 0.149429624    | 0.136376014   | 0.074102975    |

#### Testing Stage

| LightGBM_test_rmse | ensemble_test_rmse |
|-----------------|--------------------|
| 0.13791         | 0.13199            |


## References
- Packages
  - ggplot2、dplyr、magrittr、MASS、caret、janitor、fitdistrplus
  - randomForest、lightgbm、xgboost

- Related publications
  - [kaggle example](https://www.kaggle.com/serigne/stacked-regressions-top-4-on-leaderboard)
