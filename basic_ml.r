## DEPENDENCY	###################################################################################
source("https://raw.githubusercontent.com/oltkkol/vmod/master/rlibrary_dependency.r", encoding="UTF-8")

rlibrary("caTools")
rlibrary("e1071")
rlibrary("MASS")
rlibrary("datasets")
rlibrary("e1071")
rlibrary("mlbench")
rlibrary("mxnet", function(){	# see http://mxnet.incubator.apache.org/get_started/install.html
	cran <- getOption("repos")
	cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
	options(repos = cran)
	install.packages("mxnet")
})

## functions	###################################################################################

## Shuffles data frame rows
ShuffleRows	<- function(dataset){
	return ( dataset[sample(nrow(dataset)),] )
}

## Removes given columns from dataset
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
	return ( na.omit(dataset) )
}

## Removes all columns with zeros
RemoveAllZeroColumns <- function(dataset){
	return (dataset[, colSums(dataset != 0) > 0])
}

## Clears dataset: Removes all rows with NaN and all columns with 0s only
ClearDataset <- function(dataset){
	return ( RemoveAllZeroColumns(RemoveAllNanRows(dataset)) )
}

## Gets named list of X (features) and Y (targets) from given dataset
## Eg: GetXAndY(iris, "Species")
GetXAndY <- function(dataset, targetColumnName){
	X = RemoveGivenColumns(dataset, c(targetColumnName))
	Y = KeepOnlyGivenColumns(dataset, c(targetColumnName))

	return ( list(X = X, Y = Y ) )
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

## Splits dataset into train and test datasets (by trainToTestRatio) preserving class balance (by targetColumnName).
## Rows can be shuffled before. Datasets can be scaled by scaleBy parameter "none"/"minmax"/"z-score".
## Returns named list with $Train, $Test datasets with Features $X and Targets $Y names. Scaling info in $ScaleInfo.
## Eg: PrepareTrainAndTest(iris, "Species", 2/3, shuffle=T, scaleBy="z-score") 
PrepareTrainAndTest <- function(dataset, targetColumnName, trainToTestRatio=2/3, shuffle=T, scaleBy="none"){
	splitDataset <- SplitDataSetTrainTest(dataset, targetColumnName, trainToTestRatio, shuffle)

	train		<- GetXAndY(splitDataset$trainingData,	targetColumnName)
	test		<- GetXAndY(splitDataset$testingData,	targetColumnName)
	
	scaledDatasets	<- ScaleDatasets(train, test, scaleBy=scaleBy)
	return ( list(Train = scaledDatasets$Train, Test = scaledDatasets$Test, ScaleInfo = scaledDatasets$ScaleInfo ) )
}

## Scales train and test dataset by zscore/minmax w.r.t. train dataset
## Eg: scaledDatasets <- ScaleDatasets(train, test, scaleBy=scaleBy)
ScaleDatasets <- function(trainDataset, testDataset, scaleBy="z-score"){
	trainX			<- trainDataset$X
	testX			<- testDataset$X
	n				<- ncol(trainX)

	trainColsRange	<- apply(trainX, 2, FUN=function(r) max(r) - min(r) )
	trainColsMin	<- apply(trainX, 2, FUN=min)
	trainColsSd		<- apply(trainX, 2, FUN=sd)
	trainColsMean	<- apply(trainX, 2, FUN=mean)

	if ( scaleBy == "min-max" || scaleBy == "minmax" ){
		scaledTrainX	<- sapply(1:n, function(col) (trainX[,col] - trainColsMin[col])/trainColsRange[col] )
		scaledTestX		<- sapply(1:n, function(col) (testX[,col]  - trainColsMin[col])/trainColsRange[col] )

	}else if ( scaleBy == "z-score" || scaleBy == "zscore" ){
		scaledTrainX	<- sapply(1:n, function(col) (trainX[,col] - trainColsMean[col])/trainColsSd[col] )
		scaledTestX		<- sapply(1:n, function(col) (testX[,col]  - trainColsMean[col])/trainColsSd[col] )

	}else{
		scaledTrainX	<- trainX
		scaledTestX		<- testX
	}
	
	outputTrain		<- list( X = scaledTrainX,	Y = trainDataset$Y)
	outputTest		<- list( X = scaledTestX, 	Y = testDataset$Y )
	scaleInfo		<- list( Range = trainColsRange, Min = trainColsMin, Sd = trainColsSd, Mean = trainColsMean )

	return ( list(Train = outputTrain, Test = outputTest, ScaleInfo = scaleInfo  ) )
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

EvaluateModelAndPlot <- function(model, trainData, testData, newWindow=T){
	if (newWindow) x11()

	resultTrain <- EvaluateModel(model, trainData)
	resultTest	<- EvaluateModel(model, testData)

	layout(matrix(c(1,1,1,1,2,3,4,5), nrow=2, byrow=T))
	data <- round( rbind( unlist(resultTrain$Statistics), unlist(resultTest$Statistics) ), 3)
	bp <- barplot(data , beside=T, ylim=c(0,1), legend.text=c("Train", "Test"), main=model$call, col=c("grey80", "grey95"))
	text(bp, data, labels=data, pos=1)
	barplot(table(trainData$Y)/length(trainData$Y), main="Train Ys Ratios [%]")
	barplot(table(testData$Y) /length(testData$Y) , main="Test Ys Ratios [%]")
	PlotConfusionMatrix(resultTrain$Confusionmatrix, "Train Confusion")
	PlotConfusionMatrix(resultTest$Confusionmatrix, "Test Confusion")
}

PlotConfusionMatrix <- function(confusionMatrix, title){
	N <- ncol(confusionMatrix)
	x <- confusionMatrix

	image( 1:N, 1:N, -(x[, N:1]), 
			col=colorRampPalette(c(hsv(h = 0, s = 0.9, v = 0.9, alpha = 1), 
									hsv(h = 0, s = 0, v = 0.9, alpha = 1), 
									hsv(h = 2/6, s = 0.9, v = 0.9, alpha = 1)))(41), 
			xlab="", ylab='', xaxt='n', yaxt='n', main=title, zlim=c(-10, 10) )

	axis(1, at=1:N, labels=paste(colnames(x), "'", sep=""))
	axis(2, at=N:1, labels=colnames(x), las=1)
	abline(h = 0:N + 0.5, col = 'gray')
	abline(v = 0:N + 0.5, col = 'gray')
	text(1:N, rep(N:1, each=N), labels = as.vector(x))
	box(lwd=2)
}

EvaluateModelsAndGetBest <- function(models, data, metricName = "Kappa"){
	bestModel	<- NULL
	bestValue	<- 0

	for (model in models){
		value <- EvaluateModel(model, data)$Statistics[[metricName]]
		if (value > bestValue){
			bestModel <- model
			bestValue <- value
		}
	}

	return( list(Value = bestValue, Model = bestModel) )
}

## Trains and evaluates any model with train and test data (list of X, Y), returns named list with trained model & statistics
## Eg.: TrainAndEvalute(svm, train, test, kernel="linear")
TrainAndEvalute	<- function(technique, trainData, testData, ...){
	model <- technique(trainData$X, trainData$Y, ...)
	return (list(
		Model			= model,
		TrainEvaluation	= EvaluateModel(model, trainData),
		TestEvaluation	= EvaluateModel(model, testData)
	))
}

###################################################################################################
stop()

## 1. Simple Examples		#######################################################################

## IRIS
datasets	<- PrepareTrainAndTest(iris, "Species", 3/4)
train		<- datasets$Train
test		<- datasets$Test

model		<- svm(train$X, train$Y, kernel='linear')

EvaluateModel(model, train)
EvaluateModel(model, test)

EvaluateModelAndPlot(model, train, test)

## SONAR
data(Sonar)
dataset		<- Sonar
datasets	<- PrepareTrainAndTest(dataset, "Class", 2/3, scaleBy="minmax")

modelBayes	<- naiveBayes(datasets$Train$X, datasets$Train$Y)
modelSVM	<- svm(datasets$Train$X, datasets$Train$Y, kernel="linear")

EvaluateModelAndPlot(modelBayes,	datasets$Train, datasets$Test)
EvaluateModelAndPlot(modelSVM,		datasets$Train, datasets$Test)

##	2. MORE ADVANCED	###########################################################################

# TITANIC
titanic				<- read.csv("G:/VMOD/Datasety/titanic.txt")

#  - clear data
dataset				<- KeepOnlyGivenColumns( titanic, c("pclass", "survived", "sex", "age") )
dataset				<- RemoveAllNanRows(dataset)

dataset$sex			<- as.numeric(dataset$sex)
dataset$survived	<- as.factor(dataset$survived)

#  - prepare train/test
datasets	<- PrepareTrainAndTest(dataset, "survived", trainToTestRatio=2/3, scaleBy="z-score")
train		<- datasets$Train
test		<- datasets$Test

#  - train multiple models
model1		<- svm(train$X, train$Y, kernel='linear')
model2		<- svm(train$X, train$Y, kernel='radial')		
model3		<- svm(train$X, train$Y, kernel='polynomial')
model4		<- svm(train$X, train$Y, kernel='sigmoid')
model5		<- lda(train$X, train$Y)
model6		<- qda(train$X, train$Y)
model7		<- naiveBayes(train$X, train$Y)

EvaluateModelAndPlot(model1, train, test)

#  - see best model by Kappa
models			<- list(model1, model2, model3, model4, model5, model6, model7)
bestModelResult <- EvaluateModelsAndGetBest(models, train, "Kappa" )
EvaluateModelAndPlot(bestModelResult$Model, train, test)

## 3.	MORE ADVANCED 	##########################################################################
authors		<- read.table("G:/VMOD/Datasety/BOW_FoglarAsimov_1k.txt", encoding="UTF-8", sep="\t", row.names=1, header=T)
authors		<- RemoveGivenColumns(authors, c("TextName", "TextID"))	

datasets	<- PrepareTrainAndTest(authors, targetColumnName="Author", shuffle=T, trainToTestRatio=6/8, scaleBy="none")
train		<- datasets$Train
test		<- datasets$Test

modelNB		<- naiveBayes(train$X, train$Y)
EvaluateModelAndPlot(modelNB, train, test)

modelSVM	<- svm(train$X, train$Y, kernel="linear")
EvaluateModelAndPlot(modelSVM, train, test)

#  Inspect Naive Bayes:
words	<- as.data.frame(   t( sapply( modelNB$tables, function(x) x[,1])  ) )
words[ order(-words$Foglar),][1:20,]		# top 10 decisive words for Foglar
words[ order(-words$Asimov),][1:20,]		# top 10 decisive words for Asimov

## 4.	ULTRA FAST	###############################################################################

authors		<- read.csv("G:/VMOD/Datasety/Autori.txt", sep="\t")
dataset		<- KeepOnlyGivenColumns( authors, c("Author", "AVGTOKENLEN", "GINISCOEF", "L") )

datasets	<- PrepareTrainAndTest(dataset, "Author", trainToTestRatio=2/3)
train		<- datasets$Train
test		<- datasets$Test

#  - train multiple models
TrainAndEvalute(svm, train, test, kernel="linear")
TrainAndEvalute(svm, train, test, kernel="radial")
TrainAndEvalute(svm, train, test, kernel="polynomial")
TrainAndEvalute(svm, train, test, kernel="sigmoid")
TrainAndEvalute(lda, train, test)
TrainAndEvalute(naiveBayes, train, test)

output <- c()
for(z in seq(.Machine$double.eps, 2, 0.01)){
	accuracy	<- TrainAndEvalute(svm, train, test, kernel="radial", cost=z)$Train$Statistics$Accuracy
	output		<- rbind(output, c(z, accuracy))
}

plot(output, xlab="cost", ylab="Accuracy")