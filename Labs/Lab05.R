setwd("C:\\Users\\Pieter.vanSchaik\\Desktop\\Microsoft Learning\\20773\\Allfiles\\Labfiles\\Lab05")

flightData <- rxImport("FlightDelayData.xdf")
clust <- rxKmeans(~ as.numeric(CRSDepTime) + Delay ,data = flightData, numClusters = 5)
