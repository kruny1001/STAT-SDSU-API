## TODO:
## DataClass
require(data.table)
require(ggplot2)
require(RCurl)
require(caret)

### Example:
### a <- DataClass$new(targetCol="Bad", part=.8)
### a$getTrain()
### a$getTest()
### a$drawHist()
### a$testMethod()

DataClass <- setRefClass("DataClass",
    fields = list(
      train = "data.table",
      test = "data.table",
      originalData = "data.table",
      totalRecord = "numeric"
    ),
    methods = list(
      initialize = function(targetCol="Bad", part=.6){
        url <- "https://raw.githubusercontent.com/kruny1001/STAT551-Data/master/retentiondata.csv"
        myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
        data <- read.csv(textConnection(myfile), header=TRUE)
        .self$originalData = data.table(data)
        d = createDataPartition(y=data[[targetCol]], p=part, list =FALSE)
        .self$totalRecord <- nrow(data)
        .self$train <- data.table(data[d,])
        .self$test <- data.table(data[-d,])
      },
      getWholeDataset = function(){
        return(.self$originalData)
      },
      getTrain = function() {
        return(.self$train)
      },
      getTest = function() {
        return(.self$test)
      },
      getTotalNum = function(){
        return(.self$totalRecord)
      },
      drawHist = function() {
        hist(.self$train$Quarterly_Fico_Score, main="Fico Score Histogram", xlab="Quarterly Fico Score", ylim=c(0, 2000))
      },
      scotterPlot = function(DT){
        p <- ggplot(DT)
        p <- p + labs(list(title = "Quarterly Fico Score V.S. Behavior Score", x = "Quarterly Fico Score", y = "Behavior Score"))
        p <- p+ geom_point(data = subset(DT, Bad == "0"), aes(x = Quarterly_Fico_Score, y = Behavior_Score, color = factor(Bad)), color="orange", size=2)
        p <- p + geom_point(data = subset(DT, Bad == "1"), aes(x = Quarterly_Fico_Score, y = Behavior_Score, color = factor(Bad)), color="red", size=2)
        p
      }
  )
)
