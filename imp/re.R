
# Use the data from the Retention assignment and redo the assignment using a MARS model.
# MARS example code can be found in the contents tab in code labeled Lindsay and also Mars.
# Note that you have to pass the information regarding a family=binomial to make it logistic.

# The example code shows that in the mars code.
# Be sure to summarize the data, give histograms and visualization as necessary.
# Measure the accuracy of the model with a gains table as well as ROC, KS, cumulative lift as appropriate.
# I will be more 'picky' not regarding format of the report.
# You should turn in a professionally formatted report.
# Additionally a powerpoint as one or more of you will be asked to briefly present this in class.

require(data.table)
library(ggplot2)
library(RCurl)
library(earth)
library(mda)
library(faraway)
library(lattice)

loadData <- function(){
  url <- "https://raw.githubusercontent.com/kruny1001/STAT551-Data/master/retentiondata.csv"
  myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  dat <- read.csv(textConnection(myfile), header=TRUE)
  DT <- data.table(dat)
  return (DT)
}

scotterPlot <- function(DT){
  p <- ggplot(DT)
  p <- p + labs(list(title = "Quarterly Fico Score V.S. Behavior Score", x = "Quarterly Fico Score", y = "Behavior Score"))
  p <- p+ geom_point(data = subset(DT, Bad == "0"), aes(x = Quarterly_Fico_Score, y = Behavior_Score, color = factor(Bad)), color="orange", size=2)
  p <- p + geom_point(data = subset(DT, Bad == "1"), aes(x = Quarterly_Fico_Score, y = Behavior_Score, color = factor(Bad)), color="red", size=2)
  return(p)
}

histograms <- function(DT){
  par(mfrow=c(1,3))
  hist(DT$Quarterly_Fico_Score, main="Fico Score Histogram", xlab="Quarterly Fico Score", ylim=c(0, 2000))
  hist(DT$Behavior_Score, main="Behavior Score Histogram", xlab="Behavior Score", ylim=c(0, 2000))
  hist(DT$Bad, main="Bad Histogram", xlab="Bad", ylim=c(0, 7000))
}

histogramsPred <- function(DT){
  par(mfrow=c(1,2))
  hist(DT$Quarterly_Fico_Score, main="Fico Score Histogram", xlab="Quarterly Fico Score", ylim=c(0, 2000))
  hist(DT$Behavior_Score, main="Behavior Score Histogram", xlab="Behavior Score", ylim=c(0, 2000))
  #hist(DT$Bad, main="Bad Histogram", xlab="Bad", ylim=c(0, 7000))
}

histogramsDep <- function(DT){
  #hist(DT$Quarterly_Fico_Score, main="Fico Score Histogram", xlab="Quarterly Fico Score", ylim=c(0, 2000))
  #hist(DT$Behavior_Score, main="Behavior Score Histogram", xlab="Behavior Score", ylim=c(0, 2000))
  hist(DT$Bad, main="Bad Histogram", xlab="Bad", ylim=c(0, 7000))
}
#color = c("red", "blue")
scotterPlot <- function(DT){
  p <- ggplot(DT)
  p <- p + labs(list(title = "Quarterly Fico Score V.S. Behavior Score", x = "Quarterly Fico Score", y = "Behavior Score"))
  p <- p+ geom_point(data = subset(DT, Bad == "0"), aes(x = Quarterly_Fico_Score, y = Behavior_Score, color = factor(Bad)), color="orange", size=2)
  p <- p + geom_point(data = subset(DT, Bad == "1"), aes(x = Quarterly_Fico_Score, y = Behavior_Score, color = factor(Bad)), color="red", size=2)
  p
}

createPrediction <- function(data, deg){
  library(caret)
  d = createDataPartition(y=data$Bad, p =.8, list =FALSE)
  training <- data[d]
  testing <- data[-d]
  earth2=earth(Bad~Quarterly_Fico_Score+Behavior_Score, data=training, glm=list(family=binomial),degree=deg)
  predictions=predict(earth2,type='response',newdata=testing)
  testing[, prob:=predictions]
  testing = testing[order(-rank(prob))]
  return (testing)
}

createTableG <- function(passData){
  tempData <- passData
  totalPop = nrow(tempData)
  cum1 <-cumsum(tempData$Bad)
  tempData[, cum1 := cumsum(tempData$Bad)]
  list <- 1:nrow(tempData)
  tempData[, cum0 := list - tempData$cum1]
  tempData[,TP := tempData$cum1/max(cum1)]
  tempData[, FP := tempData$cum0/max(cum0)]
  tempData[, Diff := abs(tempData$TP - tempData$FP)]
  tempData[, RespRate:= tempData$cum1 / 1:nrow(tempData)]
  tempData[, cumPercentPop := 1:nrow(tempData) / totalPop]
  tempData[, positiveRate := tempData$cum1 / 1:nrow(tempData)]
  cum1Max = max(tempData$cum1)
  tempData[, cumLift := tempData$positiveRate/ (cum1Max / totalPop)]
  return (tempData)
}

drawROCCurve <- function(table){
  plot(table$FP, table$TP, main="ROC Curve", xlab="False Positive Rate", ylab="True Positive Rate", type="l", col="blue")
  abline(0,1,col="red")
  legend(0.5, 0.3, c("ROC", "Baseline"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue", "red"))
}

#KS curve
drawKSCurve <- function(table){
  row = table[which.max(table$Diff)]

  plot(table$cumPercentPop, table$FP, col="red", xlab="Cumulative Percent Population"
       , ylab="Capture Rate", type="l", main="KS Curve")
  lines(table$cumPercentPop , table$TP, col="blue")
  abline(0,1,col="green")
  segments(row$cumPercentPop, row$FP, row$cumPercentPop, row$TP,lty="dotted", col= 'black')
  points(row$cumPercentPop, row$FP, bg="grey")
  points(row$cumPercentPop, row$TP, bg="grey")
  text(row$cumPercentPop+.052, (row$FP+row$TP)/2, paste("KS", round(row$Diff, digits=3)))
  legend(0.7, 0.3, c("TPR", "FPR", paste("KS", round(row$Diff, digits=3))), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue", "red", "black"))
}


# draw the histogram with the specified number of bins
drawCumLift <- function(table){
  plot(table$cumPercentPop, table$cumLift, col="blue",
       xlab="Cumulative Percentage", ylab="Lift",
       main="Cumulative Lift", type="l")
  legend(.7, 2, c("Cumulative Lift"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue"))
}

createKSTable <- function(table){
  ksTable <- data.table(ID = c("Cumulative1", "Cumulative0", "Differences"),
                        Cumulative1 = table$TP, Cumulative0 = table$FP, Differences = table$Diff,
                        cumPercentPop = table$cumPercentPop)

  ksTable[, index:=1:nrow(ksTable)]
  return(ksTable)

}


createLiftPlot <- function(table){
  #lift
  labels = c("(0,125]","(125,250]","(250,375]","(375,500]","(500,625]", "(625,750]", "(750,875]","(875,1000]", "(1000,1125]", "(1125,1250]")
  plot(1:10, table$lift*100, type='l', main='lift', col='red', xaxt='n' ,ylim=c(0, 350))
  line(1:10, table$lift*100)
  axis(1, at=1:10, labels=labels, cex.axis=.7)

}

createCumLiftPlot <- function(table){
  #Cumlift
  labels = c("(0,125]","(125,250]","(250,375]","(375,500]","(500,625]", "(625,750]", "(750,875]","(875,1000]", "(1000,1125]", "(1125,1250]")
  plot(1:10, table$cumLift*100, main = 'Cumulative Lift', xlab = 'Cumulative Percentage',ylab = 'Lift', type = 'l', col = 'blue',  xaxt='n',ylim=c(0, 350))
  abline(h=100, col = 'red')
  axis(1, at=1:10, labels=labels, cex.axis=.7)

}

createRespPlot <- function(table){
  #Response Rate
  labels = c("(0,125]","(125,250]","(250,375]","(375,500]","(500,625]", "(625,750]", "(750,875]","(875,1000]", "(1000,1125]", "(1125,1250]")
  plot(1:10, table$responseRate*100, main = 'Cumulative Lift', xlab = 'Response Percentage', ylab = 'Lift', type = 'l', col = 'blue',  xaxt='n',ylim=c(0, 100))
  axis(1, at=1:10, labels=labels, cex.axis=.7)
}

createGains <- function(sortedData){
  list <- rep(1:nrow(sortedData), 1)
  numBy <- round(nrow(sortedData) / 10)
  dec <- cut(list, breaks = seq(0, nrow(sortedData)+numBy/2, by = numBy))
  index <- seq(1, 1, length.out = nrow(sortedData))
  sortedData[ ,dec1 := dec]
  sortedData[ ,index1 := index]
  options(scipen = 1)
  agg.sum <- aggregate(Bad~dec , data = sortedData, FUN=sum)
  agg.index <- aggregate(index~dec , data = sortedData, FUN=sum)

  cumPercent1 <- agg.sum$Bad / nrow(sortedData)
  table <- data.table( decile = agg.sum$dec,
                       numPop = agg.index$index,
                       cumPop = cumsum(agg.index$index),
                       cumPerctPop = agg.index$index / nrow(sortedData),
                       numBad = agg.sum$Bad,
                       cumBad = cumsum(agg.sum$Bad),
                       cumPercent = cumsum(agg.sum$Bad / sum(agg.sum$Bad))
  )

  responseRate <- table$numBad /table$numPop
  table[, responseRate := responseRate]
  totalResponseRate <- sum(table$numBad) / nrow(sortedData)
  lift <- table$responseRate /totalResponseRate
  table[,lift := lift]
  cumResponseRate <- table$cumBad / table$cumPop
  table[, cumResponseRate := cumResponseRate]
  cumLift <- table$cumResponseRate / totalResponseRate
  table[, cumLift := cumLift]
  return(table)
}

"
data <- loadData()
scotterPlot(data)

par(mfrow=c(1,3))
histograms(data)
histogramsPred(data)
histogramsDep(data)

predictions1 = createPrediction(data, 1)
predictions2 = createPrediction(data, 2)
gainsT1 = createTableG(predictions1)
gainsT2 = createTableG(predictions2)

par(mfrow=c(1,2))
drawROCCurve(gainsT1)
drawROCCurve(gainsT2)


par(mfrow=c(1,2))
drawKSCurve(gainsT1)
drawKSCurve(gainsT2)

par(mfrow=c(1,2))
drawCumLift(gainsT1)
drawCumLift(gainsT2)

par(mfrow=c(1,2))
KST1 = createKSTable(gainsT1)
KST2 = createKSTable(gainsT2)

max(KST1$Differences) # KS value
max(KST2$Differences) # KS value


finalT1 = createGains(gainsT1)
finalT2 = createGains(gainsT2)

par(mfrow=c(1,2))
createLiftPlot(finalT1)
createLiftPlot(finalT2)

par(mfrow=c(1,2))
createCumLiftPlot(finalT1)
createCumLiftPlot(finalT2)

par(mfrow=c(1,2))
createRespPlot(finalT1)
createRespPlot(finalT2)
"
