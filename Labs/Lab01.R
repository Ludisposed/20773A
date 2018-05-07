setwd("C:\\Users\\Pieter.vanSchaik\\Desktop\\Microsoft Learning\\20773\\Allfiles\\Labfiles\\Lab01")

#################################################################################################
# Exercisee 1

flightData <- read.csv("2000.csv")

ncol(flightData)
colnames(flightData)
min(flightData$ArrDelay, na.rm=T)
max(flightData$ArrDelay, na.rm=T)
nrow(xtabs(~Month + as.factor(Cancelled == 1), flightData))

#################################################################################################
# Exercisee 2

flightDataRx <- rxImport("2000.csv")

rxGetInfo(flightDataRx)
rxGetVarInfo(flightDataRx)

# FixIt
rxQuantile("ArrDelay", flightDataRx)
rxCrossTabs(formula = ~Month:as.factor(Cancelled == 1), data = flightDataRx)
rxCube(formula = ~Month:as.factor(Cancelled), data = flightDataRx)

#################################################################################################
# Exercisee 3

# NaN