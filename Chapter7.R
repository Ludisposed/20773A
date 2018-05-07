# Chapter 7, Decision Tree's

# Lesson 1 - Partitioning
##################################################

# rxBForest > rxDForest
# Split Data into Test and Train
diamondData <- rxDataStep(inData = ggplot2::diamonds,
                          transforms = list(set = factor(ifelse(runif(.rxNumRows) >= 0.05, "train", "test")),
                                            value = factor(ifelse(price >= 4000, "high", "low"))),
                          varsToKeep = c("cut", "clarity", "carat", "color"))


diamondDataList <- rxSplit(diamondData, splitByFactor = "set")
lapply(diamondDataList, nrow)

# Building a (D)amn Forest
# DTree/Forest
diamondTree <- rxDTree(value ~ cut + carat + color + clarity,
                       data = diamondDataList$diamondData.set.train,
                       maxDepth = 4)

diamondForest <- rxDForest(value ~ cut + carat + color + clarity,
                           data = diamondDataList$diamondData.set.train,
                           maxDepth = 4,
                           nTree = 50, mTry = 2, importance = TRUE)

diamondTree
# 2) 0.97 => Carats are high value, except lower clarity then 1.3 Carats
# 3) 0.895 - 0.97 Only if high color, clarity
# 4) 0.895 <= low value


diamondForest
# OOB estimate: 3.65% Those were wrongly classified

# Building a (B)etter Forrest
diamondBetterForrest <- rxBTrees(value ~ cut + carat + color + clarity,
                                 data = diamondDataList$diamondData.set.train,
                                 lossFunction = "bernoulli",
                                 maxDepth = 3,
                                 learningRate = 0.4,
                                 mTry = 2,
                                 nTree = 50)

diamondBetterForrest

# We gotta tune the forrest
dt1 <- rxDTree(value ~ cut + carat + color + clarity,
               data = diamondDataList$diamondData.set.train)
plotcp(rxAddInheritance(dt1))
dt2 <- prune.rxDTree(dt1, cp=0.0025)


# Lesson 2 - Prediction
###########################################################

# Make predictions
prediction <- rxDataStep(diamondDataList$diamondData.set.test,
                         varsToKeep = c("cut", "carat", "color", "clarity"))
predictionTree <- rxPredict(diamondTree, prediction, type = "class")
predictionForrest <- rxPredict(diamondForest, prediction, type = "class")

# Test how well the prediction have performed
# Original Test!
rxSummary(~value, data = diamondDataList$diamondData.set.test)
# 972 high
# 1777 low

# vs Prediction (Tree)
rxSummary(~value_Pred, data = predictionTree)
# 979 high
# 1770 low

# vs Prediction (Forrest)
rxSummary(~ value_Pred, data = predictionForrest)
# 988 high
# 1761 low

# Add the predicted value of each diamond to the in-memory data frame
predictData <- rxMerge(prediction, predictionTree, type = "oneToOne")

# Compare the predicted results against the actual values by variable
rxSummary(~ value : (color + clarity + F(carat)), data = diamondDataList$diamondData.set.test)
rxSummary(~ value_Pred : (color + clarity + F(carat)), data = predictData)

# Visualize the DTree model using RevoTreeView
library(RevoTreeView)
plot(createTreeView(diamondTree))

# Generate a line plot of the DTree model
library(rpart)
plot(rxAddInheritance(diamondTree))
text(rxAddInheritance(diamondTree))

# Show an importance plot from the DForest model
rxVarImpPlot(diamondForest)