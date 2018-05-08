setwd("C:\\Users\\Pieter.vanSchaik\\Desktop\\Microsoft Learning\\20773\\Allfiles\\Labfiles\\Lab06")

# Exersice 1
###################################################################################################

# RxXdfData!! Not use rxImport..!
flightData <- RxXdfData("FlightDelayData.xdf")
flightDelayDataSample <- rxDataStep(inData = flightData,
                                    outFile = "flightDelayDataSample.xdf",
                                    overwrite = TRUE,
                                    rowSelection = as.logical(rbinom(.rxNumRows, 
                                                                     size = 1, 
                                                                     prob = 0.01)))

# Guess the K
ratio <- vector()
for (i in 1:20) {
    cluster <- rxKmeans(formula = ~DepTime + Delay,
                        data = flightDelayDataSample,
                        transforms = list(DepTime = as.numeric(DepTime)),
                        numClusters = i,
                        seed = 12345)
    ratio <- c(ratio, cluster$betweenss / cluster$totss)
}

# Plot the ratio for each cluster
plotData <- data.frame(num = c(1:length(clusters)), ratio)
rxLinePlot(ratio ~ num, plotData, type="p")

# Exersice 2
###################################################################################################

# Pick a cluster and create a prediction using linear regression
i <- 9
testCluster <- rxKmeans(formula = ~DepTime + Delay,
                        data = flightDelayDataSample,
                        transforms = list(DepTime = as.numeric(DepTime)),
                        numClusters = i,
                        seed = 12345)

clusterModel <- rxLinMod(Delay ~ DepTime, 
                         data = as.data.frame(testCluster$centers),
                         covCoef = TRUE)

# Retrieve a random set of departure times and actual delays from the flight delay data
delayTestData <- rxDataStep(inData = flightData, 
                            varsToKeep = c("DepTime", "Delay"),
                            transforms = list(DepTime = as.numeric(DepTime)),
                            rowSelection = as.logical(rbinom(.rxNumRows, 
                                                             size = 1, 
                                                             prob = 0.01)))

# Make predictions using the linear regression model
delayPredictions <- rxPredict(clusterModel, data = delayTestData, 
                              computeStdErr = TRUE, interval = "confidence",
                              writeModelVars = TRUE)
head(delayPredictions)

# Plot the differences
rxLinePlot(formula = Delay - Delay_Pred~as.numeric(DepTime), data = delayPredictions, 
           type = c("p"), symbolStyle = c("."),
           xlab = "Departure Time",
           ylab = "Difference between Actual and Predicted Delay",
           xlim = c(0, 2400), ylim = c(-500, 1000)
)

# Exersice 3
###############################################################################################
# NVT