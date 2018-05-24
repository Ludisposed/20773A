setwd("C:\\Users\\Pieter.vanSchaik\\Documents\\R\\Kaggle\\Titanic")
library("dplyr")

test <- read.csv("test.csv")
train <- read.csv("train.csv")

# Combine the DataSets to fill in the missing Data
full <- bind_rows(test, train)

###########################################################################
# Add relevant data
# 1. Get the Titles...
# 2. Get the TravelGroups....
# 3. Get Embarked as Factor
full <- rxDataStep(inData = full,
                   transforms = list(FamilySize = as.numeric(Parch + SibSp + 1),
                                     Titles = as.factor(gsub("Don|Jonkheer|Sir", "Sir", 
                                                        gsub("Dona|Lady|Madame|the Countess", "Lady", 
                                                        gsub("^.*, (.*?)\\..*$", "\\1", Name)))),
                                     TravelGroup = match(Ticket, unique(Ticket)),
                                     Embarked = as.factor(Embarked)
                                     )
                   )

# Sort on PassengerId for missing Data
full <- arrange(full, PassengerId)

#########################################################################
# Fix the missing values
# 1. Fare
# 2. Embarked
# 3. Age

###########################
# 1. Guess the missing fare
full[which(is.na(full$Fare)), 1] # 1044
full[1044, c(2, 11)] # PClass 3 and Embarked from S
full %>% filter(Pclass == "3" & Embarked == "S") %>% summarise(missing_fare = median(Fare, na.rm = TRUE)) # 8.05
full$Fare[1044] <- 8.05

##########################
# 2. Guess the Embarked Port
full[which(is.na(full$Embarked)), 1]
full[c(62, 830), c(2, 9)] # From Class 1, with Price 80
full %>% group_by(Embarked, Pclass) %>% filter(Pclass == "1") %>% summarise(mfare = median(Fare), n = n()) # C is most likely
full$Embarked[c(62, 830)] <- "C"
full$Embarked <- droplevels(full$Embarked)

#########################
# Fill in the missing Ages
full[which(is.na(full$Age)), 1]
age_tree <- rxDTree(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Titles + FamilySize + TravelGroup,
                    data = full, 
                    rowSelection = !is.na(Age))
best_prune <- rxDTreeBestCp(age_tree)
age_tree <- prune.rxDTree(age_tree, best_prune)
predicted_age <- rxPredict(age_tree, data = full)
full$Age[is.na(full$Age)] <- predicted_age$Age_Pred[is.na(full$Age)]
summary(full$Age) # no more NA Ages

##################################################################
# Split Data back into test and train --> Afterwards split train to avoid overfitting
test <- rxDataStep(inData = full, rowSelection = is.na(Survived))
train <- rxDataStep(inData = full, rowSelection = !is.na(Survived))
trainData <- rxDataStep(inData = train,
                        transforms = list(set = factor(ifelse(runif(.rxNumRows) >= 0.2, 
                                                              "train", 
                                                              "test"))))
dataSet <- rxSplit(trainData, splitByFactor = "set")

###########################################################################
# 1. Tree
# 2. Forrest
# 3. etc...

##########################
# 1. Creating a Tree model
titanicTree <- rxDTree(Survived ~ Sex + Pclass + Fare + Age + Embarked + FamilySize + Titles + TravelGroup,
                       data = dataSet$trainData.set.train)

# Prune
titanicTree <- prune.rxDTree(titanicTree, rxDTreeBestCp(titanicTree))

# testData + Predict
testTree <- rxDataStep(inData = dataSet$trainData.set.test, 
                       varsToKeep = c("Survived", "Sex", "Pclass", "Fare", "Age", "Embarked", "FamilySize", "Titles", "TravelGroup")
                       )
predictDataTree <- rxPredict(titanicTree, data = testTree)

# Calculate the loss
print(sum(round(predictDataTree$Survived_Pred) == dataSet$trainData.set.test$Survived) / length(predictDataTree$Survived_Pred))
# 0.79%

##########################
# 2. Creating a Tree model
titanicForrest <- rxDForest(Survived ~ Sex + Pclass + Fare + Age + Embarked + FamilySize + Titles + TravelGroup, 
                            data = dataSet$trainData.set.train)

# testData + Predict
testForrest <- rxDataStep(inData = dataSet$trainData.set.test, 
                          varsToKeep = c("Survived", "Sex", "Pclass", "Fare", "Age", "Embarked", "FamilySize", "Titles", "TravelGroup")
                          )
predictDataForrest <- rxPredict(titanicForrest, data = testForrest)

# Calculate the loss
print(sum(round(predictDataForrest$Survived_Pred) == dataSet$trainData.set.test$Survived) / length(predictDataForrest$Survived_Pred))
# 0.83%

#########################
# 3. (Other) Methods

#######################################
# Create csv to be accepted by Kaggle #
#######################################

kaggleData <- rxPredict(titanicForrest, data = test)
dataFrame <- data.frame(PassengerId=test$PassengerId, Survived=round(kaggleData$Survived_Pred))
write.csv(dataFrame, "Forrest.csv", row.names = FALSE)