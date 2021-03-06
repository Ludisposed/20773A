setwd("C:\\Users\\Pieter.vanSchaik\\Desktop\\Microsoft Learning\\20773\\Allfiles\\Labfiles\\Lab04")
library("lubridate")

#################################################################################################
# Exercisee 1

airportData <- rxImport("airportData.xdf")
flightDelayData <- rxImport("FlightDelayData.xdf")

names(airportData)[1] <- "Origin"

new_origin <- rxGetVarInfo(airportData, varsToKeep = "Origin")[[1]][["levels"]]
newFlightDelayData <- rxFactors(inData = flightDelayData, 
                                factorInfo = list(Origin = list(newLevels = new_origin)),
                                overwrite = TRUE)

mergedData <- rxMerge(inData1 = airportData, 
                      inData2 = newFlightDelayData,
                      matchVars = c("Origin"), 
                      type = "inner")

##################################################################################################
# Exercise 2

dataSample <- rxDataStep(inData = mergedData, 
                         transforms = list(StandardizedDepartureTime = make_datetime( year = as.character(Year), 
                                                                                      month = Month, 
                                                                                      day = as.character(DayofMonth), 
                                                                                      hour = trunc(as.numeric(CRSDepTime) / 100), 
                                                                                      min = as.numeric(CRSDepTime) %% 100), 
                                           StandardizedArrivalTime = StandardizedDepartureTime + CRSElapsedTime * 60),
                         transformPackages = c("lubridate"),
                         numRows = 20000,
                         rowsPerRead = 5000)

tz(dataSample$StandardizedDepartureTime) <- dataSample$timezone
tz(dataSample$StandardizedArrivalTime) <- dataSample$timezone

#################################################################################################
# Exercise 3

SortedData <- rxSort(dataSample, sortByVars = c("StandardizedDepartureTime"))
