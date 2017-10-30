install.packages("caTools")
install.packages("e1071")

library('caTools')
library(MASS)
library(datasets)
library('e1071')
library(mlbench)

## Splits dataset into training and testing by ratio with class ratio preserved in both sets
## Eg: SplitDataSetTrainTest(iris, "Species", 2/3)

SplitDataSetTrainTest <- function(dataset, targetColumnName, ratio){
	mask <- sample.split( dataset[,targetColumnName], SplitRatio=ratio, group=dataset[,targetColumnName] )

	trainingDataSet	<- dataset[which(mask == TRUE),]
	testingDataSet	<- dataset[which(mask == FALSE),]

	return ( list(trainingData = trainingDataSet, testingData = testingDataSet) ) 
}

RemoveGivenColumns <- function(dataset, columnNames){
	return( dataset[ , -which(names(dataset) %in% columnNames)] )
}

## Gets dataset with only given columns
## Eg:  LetOnlyGivenColumns( TITANIC, c("pclass", "survived", "sex", "age") )
LetOnlyGivenColumns <- function(dataset, columnNames){
	return( dataset[ , which(names(dataset) %in% columnNames)] )
}

## Removes all rows containing NaN in any column
RemoveAllNanRows <- function(dataset){
	return( dataset[complete.cases(dataset), ] )
}

## Eg: GetXAndY(iris, "Species")
GetXAndY <- function(dataset, targetColumnName){
	
	X = RemoveGivenColumns(dataset, c(targetColumnName))
	Y = LetOnlyGivenColumns(dataset, c(targetColumnName))

	return (list(X = X, Y = Y ) )
}

GetConfusionMatrix <- function(model, testData){
	yHat <- predict(model, testData$X)

	return (table(testData$Y, yHat) )
}

CalculateStatisticsForConfusionMatrix <- function(cm){
	n			= sum(cm) # number of instances
	nc			= nrow(cm) # number of classes
	diag		= diag(cm) # number of correctly classified instances per class 
	rowsums 	= apply(cm, 1, sum) # number of instances per class
	colsums 	= apply(cm, 2, sum) # number of predictions per class
	p = rowsums / n # distribution of instances over the actual classes
	q = colsums / n # distribution of instances over the predicted classes

	accuracy		= sum(diag) / n 
	precision		= diag / colsums 
	recall			= diag / rowsums 
	f1				= 2 * precision * recall / (precision + recall) 

	expAccuracy		= sum(p*q)
	kappa			= (accuracy - expAccuracy) / (1 - expAccuracy)

	return (
		list( Accuracy		= accuracy,
			PrecisionMean	= mean(precision),
			RecallMean		= mean(recall),
			F1Mean			= mean(f1),
			Kappa			= kappa)
	)
}

###################################################################################################

dataset				<- iris				## <== YOUR DATASET
targetColumnName	<- "Species"		## <== YOUR TARGET COLUMN
trainToTestRatio	<- 2/3

## Hard Work	
splitDataset		<- SplitDataSetTrainTest(dataset, targetColumnName, trainToTestRatio)
trainDataset		<- splitDataset$trainingData
testDataset			<- splitDataset$testingData

train		<- GetXAndY(trainDataset, targetColumnName)
test		<- GetXAndY(testDataset, targetColumnName)

## Now have fun:
model		<- svm(train$X, train$Y, kernel='linear')
model		<- svm(train$X, train$Y, kernel='radial')		
model		<- svm(train$X, train$Y, kernel='polynomial')
model		<- svm(train$X, train$Y, kernel='sigmoid')		## kernely viz Google e1071

model		<- lda(train$X, train$Y)
model		<- qda(train$X, train$Y)

model		<- naiveBayes(train$X, train$Y)

# Performance:
cm <- GetConfusionMatrix(model, train)
cm <- GetConfusionMatrix(model, test)

CalculateStatisticsForConfusionMatrix(cm)
