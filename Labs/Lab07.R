setwd("C:\\Users\\Pieter.vanSchaik\\Desktop\\Microsoft Learning\\20773\\Allfiles\\Labfiles\\Lab07")

# Read file && Import
flightDelayData <- RxXdfData("FlightDelayData.xdf")
flightData <- rxDataStep(inData = flightDelayData,
                         transforms = list(DepTime = as.numeric(DepTime),
                                           ArrTime = as.numeric(ArrTime),
                                           Dataset = factor(ifelse(runif(.rxNumRows) >= 0.05, "train", "test"))),
                         varsToKeep = c("Delay", "Origin", "Month", "DayOfWeek", "DepTime", "ArrTime",
                                        "Dest", "Distance", "UniqueCarrier", "OriginState", "DestState"))

# Split datasets into test and train + create DTree
flightDataList <- rxSplit(flightData, splitByFactor = "Dataset")
flightDataTree <- rxDTree(Delay ~ DepTime + ArrTime + Month + DayOfWeek,
                          data = flightDataList$flightData.Dataset.train,
                          maxDepth = 4)

# Visualize tree
flightDataTree
library(RevoTreeView)
plot(createTreeView(flightDataTree))

#----> OVERFITTING,  we need to tune the Tree
# ???
# ???
# ???

# Make predictions
testData <- rxDataStep(inData = flightDataList$flightData.Dataset.test,
                       varsToKeep = c("Delay", "DepTime", "ArrTime", "Month", "DayOfWeek"))
predictData <- rxPredict(flightDataTree, data = testData)

# Get flights inbetween minutes
processMinRange <- function(predictData, minRange) {
    matches <- sum(abs(predictData$Delay_Pred - testData$Delay) <= minRange, na.rm = TRUE)
    percentage <- (matches / nrow(predictData)) * 100
    print(sprintf("%d predictions were within %d percent of the actual delay time. This is %f percent of the total", matches, minRange, percentage))
}
processMinRange(predictData, 10)

# Get flights in between percentage
processPercentage <- function(predictData, perc) {
    matches <- sum(abs(predictData$Delay_Pred - testData$Delay) / abs(testData$Delay) <= perc, na.rm = TRUE)
    percentage <- (matches / nrow(predictData)) * 100
    print(sprintf("%d predictions were within %f percent of the actual delay time. This is %f percent of the total", matches, perc, percentage))
}
processPercentage(predictData, 0.05)
processPercentage(predictData, 0.1)
processPercentage(predictData, 0.5)


# Exersice 2
########################################################################################################

# Repeat for forrest
flightDataForrest <- rxDForest(Delay ~ DepTime + ArrTime + Month + DayOfWeek,
                               data = flightDataList$flightData.Dataset.train,
                               maxDepth = 7)

predictionData <- rxPredict(flightDataForrest, data = testData)
processMinRange(predictionData, 10)
processPercentage(predictionData, 0.05)
processPercentage(predictionData, 0.1)
processPercentage(predictionData, 0.5)

# Exersice 3
########################################################################################################
flightDataTreeDiff <- rxDTree(formula = Delay ~ Origin + Dest + Distance + UniqueCarrier + OriginState + DestState,
                              data = flightDataList$flightData.Dataset.train,
                              maxDepth = 7)

# Create a new set of test data with the specified variables
testData <- rxDataStep(flightDataList$flightData.Dataset.test,
                       varsToKeep = c("Origin", "Dest", "Distance", "UniqueCarrier", "OriginState", "DestState"))

preData <- rxPredict(flightDataTreeDiff, data = testData)