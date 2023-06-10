library(shiny)
library(DT)
library(ggplot2)

# Data
train_df <- read.csv('data/train.csv')
# Filter out numeric columns
cat_cols <- setdiff(colnames(train_df), c("Id","SalePrice","LotFrontage",
                                          "LotArea","YearBuilt","YearRemodAdd","MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF",
                                          "1stFlrSF","2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","TotRmsAbvGrd",
                                          "Fireplaces","GarageYrBlt","GarageCars","GarageArea","WoodDeckSF","OpenPorchSF","EnclosedPorch","3SsnPorch","ScreenPorch","PoolArea","MiscVal"
                                          ))
Vars <- setdiff(colnames(train_df), c("Id","SalePrice","MSSubClass","MSZoning","OverallQual","OverallCond",
                                      "Street","Alley","LotShape","LandContour","Utilities","LotConfig","LandSlope",
                                      "Neighborhood","Condition1","Condition2","BldgType","HouseStyle","RoofStyle","RoofMatl","Exterior1st","Exterior2nd",
                                      "MasVnrType","ExterQual","ExterCond","Foundation","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","Heating",
                                      "HeatingQC","CentralAir","Electrical","KitchenQual","Functional","FireplaceQu","GarageType","GarageFinish",
                                      "GarageQual","GarageCond","PavedDrive","PoolQC","Fence","MiscFeature","SaleType","SaleCondition","MoSold","YrSold"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
    title = "Group4",
    tabPanel(
      "訓練資料", 
      tabsetPanel(
        tabPanel(
          "Raw Data",
          tags$div(
            DT::dataTableOutput("trainTable")
          )
        ),
        tabPanel(
          "Categorical features",
          selectInput("Cat", "Select Category:",
                      choices = cat_cols),
          plotOutput("brandBar")
        ),
        tabPanel(
          "Summary",
          verbatimTextOutput("summaryOutput")
        ),
        tabPanel(
          "Histogram of SalePrice",
          plotOutput("histogram")
        ),
        tabPanel(
          "scatterPlot",
          selectInput("Var", "Select Vars:",
                      choices = Vars),
          plotOutput("scatterPlot")
        ),
        tabPanel(
          "Missing Data Summary",
          DT::dataTableOutput("missingDataSummary")
        )
        
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  options(scipen = 10)  # 设置scipen为较大的值
  output$trainTable <- DT::renderDataTable({
    DT::datatable(train_df)
  })
  
  output$brandBar <- renderPlot({
    ggplot(train_df, aes_string(x = input$Cat, fill = input$Cat)) +
      geom_bar() +
      labs(x = input$Cat, y = "Count", title = paste("Distribution of", input$Cat)) +
      theme(legend.position = "none")  # Remove legend
    
  })
  
  output$summaryOutput <- renderPrint({
    summary(train_df$SalePrice)
  })
  
  output$histogram <- renderPlot({
    hist(train_df$SalePrice, freq = FALSE, col = "darkgray", border = "white",
         xlab = "SalePrice", ylab = "Density",
         main = "Histogram of SalePrice",
         ylim = c(0, 0.000008))  # 调整高度为400
    
    dens <- density(train_df$SalePrice)
    lines(dens, col = "blue")
  })
  
  output$scatterPlot <- renderPlot({
    var <- input$Var
    data <- data.frame(SalePrice = train_df$SalePrice, GrLivArea = train_df[[var]])
    
    # Create the scatter plot
    ggplot(data, aes(x = GrLivArea, y = SalePrice)) +
      geom_point(color = "blue") +
      labs(x = var, y = 'SalePrice') +
      ggtitle('Scatter plot of GrLivArea and SalePrice')
  })
  
  output$missingDataSummary <- DT::renderDataTable({
    # Compute total and percentage of missing data
    total <- colSums(is.na(train_df))
    percent <- total / nrow(train_df) * 100
    
    # Create dataframe for missing data summary
    missing_data <- data.frame(Total = total, Percent = percent)
    missing_data <- missing_data[order(-missing_data$Total), ]
    
    # Display missing data summary
    DT::datatable(missing_data, options = list(pageLength = 10))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
