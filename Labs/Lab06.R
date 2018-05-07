setwd("C:\\Users\\Pieter.vanSchaik\\Desktop\\Microsoft Learning\\20773\\Allfiles\\Labfiles\\Lab06")

flightData <- rxImport("FlightDelayData.xdf")

flightDelayDataSample <- rxDataStep(inData = flightData,
                                    outFile = "sampleData.xdf", overwrite = TRUE,
                                    rowSelection = rbinom(.rxNumRows, size = 1, prob = 0.10))


























#################################################################################################
# Plot delays as a function of departure time
rxLinePlot(formula = Delay~as.numeric(DepTime), data = flightDelayDataSample, 
           type = c("p", "r"), symbolStyle = c("."), alpha = 1/50,
           lineColor = "red", 
           xlab = "Departure Time",
           ylab = "Delay (mins)",
           xlim = c(0, 2400), ylim = c(-120, 1000))

# Expression to factorize the departure time
depTimeFactor <- expression(list(DepTime = cut(as.numeric(DepTime),  
                                               breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100,
                                                          1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400),
                                               labels = c("Midnight-1:00AM", "1:01-2:00AM", "2:01-3:00AM", "3:01-4:00AM", "4:01-5:00AM",
                                                          "5:01-6:00 AM", "6:01-7:00AM", "7:01-8:00AM", "8:01-9:00AM", "9:01-10:00AM", "10:01-11:00AM",
                                                          "11:01-Midday", "12:01-1:00PM", "1:01-2:00PM", "2:01-3:00PM", "3:01-4:00PM", "4:01-5:00PM",
                                                          "5:01-6:00PM", "6:01-7:00PM", "7:01-8:00PM", "8:01-9:00PM", "9:01-10:00PM", 
                                                          "10:01-11:00PM", "11:01PM-Midnight"))))

# Create a histogram showing the number of departures by time
# using the departure time factors as bins
rxHistogram(formula = ~DepTime, data = flightDelayDataSample,
            histType = "Counts",
            xlab = "Departure Time",
            ylab = "Number of Departures",
            transforms = depTimeFactor)

#########################################################3333

delayCluster <- rxKmeans(formula = ~DepTime + Delay,
                         data = flightDelayDataSample,
                         transforms = list(DepTime = as.numeric(DepTime)),
                         numClusters = 12)