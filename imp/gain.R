### Class Name: GainClass
### data: Sorted Table is required
## TODO:
## GainClass


GainClass <- setRefClass("GainClass",
  fields = list(
    predData = "data.table",
    decile = "list"
  ),
  methods = list(
      initialize = function(){
      },
      createDecile = function(sortedDT){
        cat("[Process-GainClass_createDecile]\n")
      },
      getDecile = function(decile){
        return(.self$decile)
      },
      createTableG = function(passData){
        cat("[Process-GainClass_createTableG]\n")
        tempData = data.table(passData)
        totalPop = nrow(tempData)
        cum1 <-cumsum(tempData$Bad)
        tempData[, cum1 := cumsum(tempData$Bad)]
        list <- 1:nrow(tempData)
        tempData[, cum0 := list - tempData$cum1]
        tempData[, TP := tempData$cum1/max(cum1)]
        tempData[, FP := tempData$cum0/max(cum0)]
        tempData[, Diff := abs(tempData$TP - tempData$FP)]
        tempData[, RespRate:= tempData$cum1 / 1:nrow(tempData)]
        tempData[, cumPercentPop := 1:nrow(tempData) / totalPop]
        tempData[, positiveRate := tempData$cum1 / 1:nrow(tempData)]
        cum1Max = max(tempData$cum1)
        tempData[, cumLift := tempData$positiveRate/ (cum1Max / totalPop)]
        return (tempData)
      },
      createGains = function(sortedData){
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
        labels = c("(0,125]","(125,250]","(250,375]","(375,500]","(500,625]", "(625,750]", "(750,875]","(875,1000]", "(1000,1125]", "(1125,1250]")
        table <- data.table( decile = labels,
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
      },
      # Get Gain Table
      getGainTable = function(){
        return(.self$gainTable)
      },
      # Draw ROC
      drawROCCurve = function(table){
        plot(table$FP, table$TP, main="ROC Curve", xlab="False Positive Rate", ylab="True Positive Rate", type="l", col="blue")
        abline(0,1,col="red")
        legend(0.5, 0.3, c("ROC", "Baseline"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue", "red"))
      },
      # KS curve
      drawKSCurve = function(table, form){
        row = table[which.max(table$Diff)]
        plot(table$cumPercentPop, table$FP, col="red", xlab="Cumulative Percent Population"
             , ylab="Capture Rate", type="l", main=paste(format(form)), cex=0.7)
        lines(table$cumPercentPop , table$TP, col="blue")
        abline(0,1,col="green")
        segments(row$cumPercentPop, row$FP, row$cumPercentPop, row$TP,lty="dotted", col= 'black')
        points(row$cumPercentPop, row$FP, bg="grey")
        points(row$cumPercentPop, row$TP, bg="grey")
        text(row$cumPercentPop+.052, (row$FP+row$TP)/2, paste("KS", round(row$Diff, digits=3)))
        legend(0.6, 0.3, cex=.5, c("TPR", "FPR", paste("KS", round(row$Diff, digits=3))), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue", "red", "black"))
      },
      # Cumulative Lift from all data
      drawCumLift = function(table, table2, table3, ...){
        plot(1:10, table$cumLift, col="blue",
             xlab="Cumulative Percentage", ylab="Lift",
             main="Cumulative Lift", type="l")
        lines(1:10, table2$cumLift, col="orange")
        lines(1:10, table3$cumLift, col="red")
        legend(.7, 2, c("Cumulative Lift"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue"))
      },
      # Lift
      createLiftPlot = function(table, table2, table3, table4){
        #lift
        labels = c("(0,125]","(125,250]","(250,375]","(375,500]","(500,625]", "(625,750]", "(750,875]","(875,1000]", "(1000,1125]", "(1125,1250]")
        plot(1:10, table$lift*100, type='l', main='lift', col='red', xaxt='n' ,ylim=c(0, 350))
        lines(1:10, table2$lift*100, type='l', col='blue')
        lines(1:10, table3$lift*100, type='l', col='brown')
        lines(1:10, table4$lift*100, type='l', col='orange')
        axis(1, at=1:10, labels=labels, cex.axis=.7)

      },
      # Comulative Lift from decile
      createCumLiftPlot = function(table){
        #Cumlift
        labels = c("(0,125]","(125,250]","(250,375]","(375,500]","(500,625]", "(625,750]", "(750,875]","(875,1000]", "(1000,1125]", "(1125,1250]")
        plot(1:10, table$cumLift*100, main = 'Cumulative Lift', xlab = 'Cumulative Percentage',ylab = 'Lift', type = 'l', col = 'blue',  xaxt='n',ylim=c(0, 350))
        abline(h=100, col = 'red')
        axis(1, at=1:10, labels=labels, cex.axis=.7)

      },
      # Response Rate
      createRespPlot = function(table, table2){
        labels = c("(0,125]","(125,250]","(250,375]","(375,500]","(500,625]", "(625,750]", "(750,875]","(875,1000]", "(1000,1125]", "(1125,1250]")
        plot(1:10, table$responseRate*100, main = 'Cumulative Lift', xlab = 'Response Percentage', ylab = 'Lift', type = 'l', col = 'blue',  xaxt='n',ylim=c(0, 100))
        lines(1:10, table2$responseRate*100, col="orange")
        axis(1, at=1:10, labels=labels, cex.axis=.7)
      },
      createKSTable = function(table){
        ksTable <- data.table(ID = c("Cumulative1", "Cumulative0", "Differences"),
                              Cumulative1 = table$TP, Cumulative0 = table$FP, Differences = table$Diff,
                              cumPercentPop = table$cumPercentPop)

        ksTable[, index:=1:nrow(ksTable)]
        return(ksTable)
      }
  )
)
