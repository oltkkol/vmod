## dependency	###################################################################################
rlibrary <- function(libraryName){
	prequire <- function() return(require(libraryName, character.only=T))

	if (prequire() == F){
		install.packages(libraryName)
		library(libraryName)
	}
}

rlibrary("caTools")
rlibrary("e1071")
rlibrary("MASS")
rlibrary("datasets")
rlibrary("e1071")
rlibrary("mlbench")

## functions	###################################################################################

## Shuffles data frame rows
ShuffleRows	<- function(dataset){
	return ( dataset[sample(nrow(dataset)),] )
}

## Splits dataset into training and testing by ratio with class ratio preserved in both sets
## Eg:	SplitDataSetTrainTest(iris, "Species", 2/3)
SplitDataSetTrainTest <- function(dataset, targetColumnName, ratio, shuffle=T){
	if (shuffle == T){
		dataset = ShuffleRows(dataset)
	}

	mask <- sample.split( dataset[,targetColumnName], SplitRatio=ratio, group=dataset[,targetColumnName] )

	trainingDataSet	<- dataset[which(mask == TRUE),]
	testingDataSet	<- dataset[which(mask == FALSE),]

	return ( list(trainingData = trainingDataSet, testingData = testingDataSet) ) 
}

## Removes given column from dataset
## Eg:	RemoveGivenColumns(iris, c("Species"))
RemoveGivenColumns <- function(dataset, columnNames){
	return( dataset[ , -which(names(dataset) %in% columnNames)] )
}

## Gets dataset with only given columns
## Eg:  KeepOnlyGivenColumns( TITANIC, c("pclass", "survived", "sex", "age") )
KeepOnlyGivenColumns <- function(dataset, columnNames){
	return( dataset[ , which(names(dataset) %in% columnNames)] )
}

## Removes all rows containing NaN in any column
RemoveAllNanRows <- function(dataset){
	#return( dataset[complete.cases(dataset), ] )
	return ( na.omit(dataset) )
}

## Gets named list of X (features) and Y (targets) from given dataset
## Eg: GetXAndY(iris, "Species")
GetXAndY <- function(dataset, targetColumnName){
	X = RemoveGivenColumns(dataset, c(targetColumnName))
	Y = KeepOnlyGivenColumns(dataset, c(targetColumnName))

	return (list(X = X, Y = Y ) )
}

## Splits dataset into train and test datasets (with trainToTestRatio) preserving class balance (targetColumnName)
## and then returning named list with Train and Test datasets with Features X and Targets Y names.
## Eg: PrepareTrainAndTest(iris, "Species", 2/3) 
PrepareTrainAndTest <- function(dataset, targetColumnName, trainToTestRatio=2/3, shuffle=T){
	splitDataset <- SplitDataSetTrainTest(dataset, targetColumnName, trainToTestRatio, shuffle)

	train		<- GetXAndY(splitDataset$trainingData,	targetColumnName)
	test		<- GetXAndY(splitDataset$testingData,	targetColumnName)
	
	return ( list(Train = train, Test = test) )
}

## Calculates confusion table for given model and given test data
PredictAndMakeConfussionMatrix <- function(model, testData){
	yHat <- predict(model, testData$X)

	if ("class" %in% names( yHat )){
		yHat <- yHat$class
	}

	return ( table(testData$Y, yHat) )
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

## Calculates confusion matrix and all other statistics for model and testing data
EvaluateModel <- function(model, testData){
	cm = PredictAndMakeConfussionMatrix(model, testData)
	return (
		list(
			Confusionmatrix	= cm,
			Statistics		= CalculateStatisticsForConfusionMatrix(cm)
		)
	)
}

EvaluateModelAndPlot <- function(model, trainData, testData){
	resultTrain <- EvaluateModel(model, trainData)
	resultTest	<- EvaluateModel(model, testData)

	 barplot(rbind( unlist(resultTrain$Statistics), unlist(resultTest$Statistics) ), beside=T, ylim=c(0,1), legend.text=c("Train", "Test"))
}

## Trains and evaluates any model with train and test data (list of X, Y), return named list with statistics for both.
## Eg.: TrainAndEvalute(svm, train, test, kernel="linear")
TrainAndEvalute	<- function(technique, trainData, testData, ...){
	model <- technique(trainData$X, trainData$Y, ...)
	return (list(
		Train	= EvaluateModel(model, trainData),
		Test	= EvaluateModel(model, testData)
	))
}

###################################################################################################

## 1. Simple Examples		#######################################################################

## IRIS
datasets	<- PrepareTrainAndTest(iris, "Species", 2/3)
train		<- datasets$Train
test		<- datasets$Test

model		<- svm(train$X, train$Y, kernel='linear')

EvaluateModel(model, train)
EvaluateModel(model, test)

EvaluateModelAndPlot(model, train, test)

## SONAR
data(Sonar)
dataset		<- Sonar
datasets	<- PrepareTrainAndTest(dataset, "Class", 2/3)

model		<- naiveBayes(datasets$Train$X, datasets$Train$Y)

EvaluateModelAndPlot(model, datasets$Train, datasets$Test)

##	2. MORE ADVANCED	###########################################################################

# TITANIC
titanic				<- read.csv("G:/VMOD/titanic.txt")

# - clear dataset -
dataset				<- KeepOnlyGivenColumns( titanic, c("pclass", "survived", "sex", "age") )
dataset				<- RemoveAllNanRows(dataset)
dataset$sex			<- as.numeric(dataset$sex)

# - prepare dataset -
datasets	<- PrepareTrainAndTest(dataset, "survived", trainToTestRatio=2/3)
train		<- datasets$Train
test		<- datasets$Test

## - train multiple models -
model1		<- svm(train$X, train$Y, kernel='linear')
model2		<- svm(train$X, train$Y, kernel='radial')		
model3		<- svm(train$X, train$Y, kernel='polynomial')
model4		<- svm(train$X, train$Y, kernel='sigmoid')

model5		<- lda(train$X, train$Y)
model6		<- qda(train$X, train$Y)

model7		<- naiveBayes(train$X, train$Y)

# - evaluate -
EvaluateModel(model1, train)
EvaluateModel(model1, test)

EvaluateModelAndPlot(model1, train, test)

## 3.	ULTRA FAST	###############################################################################

authors		<- read.csv("G:/VMOD/Autori.txt", sep="\t")
dataset		<- KeepOnlyGivenColumns( authors, c("Author", "AVGTOKENLEN", "GINISCOEF", "L") )

datasets	<- PrepareTrainAndTest(dataset, "Author", trainToTestRatio=2/3)
train		<- datasets$Train
test		<- datasets$Test

TrainAndEvalute(svm, train, test, kernel="linear")
TrainAndEvalute(svm, train, test, kernel="radial")
TrainAndEvalute(svm, train, test, kernel="polynomial")
TrainAndEvalute(svm, train, test, kernel="sigmoid")
TrainAndEvalute(lda, train, test)
TrainAndEvalute(naiveBayes, train, test)

output <- c()
for(z in seq(.Machine$double.eps, 2, 0.01)){
	accuracy <- TrainAndEvalute(svm, train, test, kernel="radial", cost=z)$Train$Statistics$Accuracy
	output <- rbind(output, c(z, accuracy))
}

plot(output, xlab="cost", ylab="Accuracy")
