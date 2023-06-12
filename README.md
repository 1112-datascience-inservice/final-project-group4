# [Group4] your projrct title
Predict House Sales Prices

## Contributors
|組員|系級|學號|工作分配|
|孫詩傑|資科碩專二|110971006|程式碼建構、簡報製作|
|xxx|資科碩專二|xxx|團隊中的吉祥物🦒，負責增進團隊氣氛| 
|xxx|資科碩專二|xxx|團隊的中流砥柱，一個人打十個|
|xxx|資科碩專二|xxx|團隊的中流砥柱，一個人打十個|

## Quick start
You might provide an example commend or few commends to reproduce your analysis, i.e., the following R script
```R
Rscript code/your_script.R --input data/training --output results/performance.tsv
```

## Folder organization and its related description
idea by Noble WS (2009) [A Quick Guide to Organizing Computational Biology Projects.](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1000424) PLoS Comput Biol 5(7): e1000424.

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
- 本次採用3模型進行Stacking，分別是**Linear Regression**、**Random Forest**以及**XGBoost**，透過k-fold方式計算各模型**RMSE**數值，並作為最終計算權重

### results
* What is a null model for comparison?
* How do your perform evaluation?
  * Cross-validation, or extra separated data

## References

* Packages you use
* Related publications
