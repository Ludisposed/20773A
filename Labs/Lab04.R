setwd("C:\\Users\\Pieter.vanSchaik\\Desktop\\Microsoft Learning\\20773\\Allfiles\\Labfiles\\Lab04")
library("lubridate")

rxSetComputeContext("local")

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

dataSam <- rxDataStep(inData = mergedData,
                      numRows = 20000,
                      rowsPerRead = 5000,
                      transforms = list(StanderdizedDepartureTime = make_datetime(year = Year, 
                                                                                  month = as.numeric(Month),
                                                                                  day = DayofMonth,
                                                                                  hour = trunc(as.numeric(CRSDepTime) / 100),
                                                                                  min = as.numeric(CRSDepTime) %% 100)),
                      transformPackages = c("lubridate"))

#################################################################################################
# Exercise 3


dataSam <- rxDataStep(inData = mergedData,
                      numRows = 20000,
                      transforms = list(StanderdizedDepartureTime = make_datetime(year = as.character(Year), 
                                                                                  month = Month,
                                                                                  day = as.character(DayofMonth),
                                                                                  hour = trunc(as.numeric(DepTime) / 100),
                                                                                  min = trunc(as.numeric(DepTime) %% 100),
                                                                                  tz = timezone),
                                        StanderdizedArrivalTime = make_datetime(year = Year, 
                                                                                month = Month,
                                                                                day = DayofMonth,
                                                                                hour = trunc(as.numeric(ArrTime) / 100),
                                                                                min = trunc(as.numeric(ArrTime) %% 100),
                                                                                tz = timezone)),
                      transformPackages = c("lubridate"))
