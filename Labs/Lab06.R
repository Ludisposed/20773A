setwd("C:\\Users\\Pieter.vanSchaik\\Desktop\\Microsoft Learning\\20773\\Allfiles\\Labfiles\\Lab06")

flightData <- rxImport("FlightDelayData.xdf")
flightDelayDataSample <- rxDataStep(inData = flightData,
                                    rowSelection = rbinom(.rxNumRows, 
                                                          size = 1, 
                                                          prob = 0.10))
                                    

# Errors!
# --> Guess k == 12

#########################################################

delayCluster <- rxKmeans(formula = ~DepTime + Delay,
                         data = flightDelayDataSample,
                         transforms = list(DepTime = as.numeric(DepTime)),
                         numClusters = 12)

# Examine the clusters
delayCluster$betweenss / delayCluster$totss
delayCluster$centers
#
#
#
#
#

