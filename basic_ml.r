## DEPENDENCY	###################################################################################
source("https://raw.githubusercontent.com/oltkkol/vmod/master/rlibrary_dependency.r", encoding="UTF-8")

## functions	###################################################################################

## L2 Norm
l2 <- function(r) sqrt( sum(r**2) )
l2Norm <- function(r) r/l2(r)

## Shuffles data frame rows
ShuffleRows	<- function(dataset){
	return ( dataset[sample(nrow(dataset)),] )
}

## Removes given columns from dataset
## Eg:	RemoveGivenColumns(iris, c("Species"))
RemoveGivenColumns <- function(dataset, columnNames){
	return( dataset[ , -which(colnames(dataset) %in% columnNames)] )
}

## Gets dataset with only given columns
## Eg:  KeepOnlyGivenColumns( TITANIC, c("pclass", "survived", "sex", "age") )
KeepOnlyGivenColumns <- function(dataset, columnNames){
	return( dataset[ , which(colnames(dataset) %in% columnNames)] )
}

## Keeps columns in target dataset as in source dataset
KeepOnlyGivenColumnsAsIn <- function(sourceDataset, targetDataset){
	return ( KeepOnlyGivenColumns(targetDataset, colnames(sourceDataset))  )
}

## Removes all rows containing NaN in any column
RemoveAllNanRows <- function(dataset){
	return ( na.omit(dataset) )
}

## Removes all columns with zeros
RemoveAllZeroColumns <- function(dataset){
	return (dataset[, colSums(dataset != 0) > 0])
}

## Removes all columns with zeros
RemoveAllZeroRows <- function(dataset){
	return (dataset[rowSums(dataset != 0) > 0, ])
}

RemoveAllZeroVarianceRows <- function(dataset){
	rowVariances	<- apply(dataset, 1, var)
	nonZeroRows		<- which( rowVariances != 0 )
	
	return ( dataset[nonZeroRows, ] ) 
}

RemoveAllZeroVarianceCols <- function(dataset){
	colVariances	<- apply(dataset, 2, var)
	nonZeroCols		<- which( colVariances != 0 )

	return ( dataset[,nonZeroCols ] ) 
}

## Clears dataset: Removes all rows with NaN and all columns with 0s only
ClearDataset <- function(dataset){
	return ( RemoveAllZeroColumns(RemoveAllNanRows(dataset)) )
}

## Gets named list of X (features) and Y (targets) from given dataset
## Eg: GetXAndY(iris, "Species")
GetXAndY <- function(dataset, targetColumnName){
	X = RemoveGivenColumns(dataset, c(targetColumnName)) 
	X = apply(X, 2, FUN=as.numeric)
	Y = as.factor(  KeepOnlyGivenColumns(dataset, c(targetColumnName)) )

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

## Returns dataset with balanced classes
## Eg: ClassesToSameCount(iris[-(1:45), ], "Species")
ClassesToSameCount <- function(dataset, targetColumnName){
	newDataSet	<- data.frame()
	classes		<- as.factor( dataset[, targetColumnName] )
	leastSize	<- min( table(classes) )

	for(c in levels(classes)){
		classRows	<- which(classes == c)
		toLet		<- classRows[ sample( leastSize) ]

		newDataSet	<- rbind(newDataSet, dataset[toLet,])
	}

	return( newDataSet )
}

## Splits dataset into train and test datasets (by trainToTestRatio) preserving class balance (by targetColumnName).
## Rows can be shuffled before. Datasets can be scaled by scaleBy parameter "none"/"minmax"/"z-score".
## Columns with zero variances in training dataset are removed from both sets.
## Returns named list with $Train, $Test datasets with Features $X and Targets $Y names. Scaling info in $ScaleInfo.
## Eg: PrepareTrainAndTest(iris, "Species", 2/3, shuffle=T, scaleBy="z-score") 
PrepareTrainAndTest <- function(dataset, 
								targetColumnName, 
								trainToTestRatio=2/3, 
								shuffle=T, 
								scaleBy="none", 
								convertToFactors=F, 
								allClassesSameSize=T){
	
	if (allClassesSameSize == T){
		dataset <- ClassesToSameCount(dataset, targetColumnName)
	}
	
	splitDataset <- SplitDataSetTrainTest(dataset, targetColumnName, trainToTestRatio, shuffle)

	train		<- GetXAndY(splitDataset$trainingData,	targetColumnName)
	test		<- GetXAndY(splitDataset$testingData,	targetColumnName)
	
	# remove zero value features (caused eg. by train/test split), apply to test
	newTrainX		<- RemoveAllZeroColumns(train$X)
	newTestX		<- KeepOnlyGivenColumns(test$X, colnames(newTrainX))

	train$X			<- as.data.frame( newTrainX )
	test$X			<- as.data.frame( newTestX )

	# scale
	scaledDatasets	<- ScaleDatasets(train, test, scaleBy=scaleBy)
	train			<- scaledDatasets$Train
	test			<- scaledDatasets$Test

	if (convertToFactors == T){
		train$X		<- DataFrameToFactor(train$X)
		test$X		<- DataFrameToFactor(test$X)
	}

	# remove zero variance features (caused eg. by train/test split), apply to test
	train$X <- RemoveAllZeroVarianceCols(train$X)
	test$X  <- KeepOnlyGivenColumns(test$X, colnames(train$X))

	return ( list(Train = train, Test = test, ScaleInfo = scaledDatasets$ScaleInfo ) )
}

## Scales train and test dataset by zscore/minmax w.r.t. train dataset
## Eg: scaledDatasets <- ScaleDatasets(train, test, scaleBy=scaleBy)
ScaleDatasets <- function(trainDataset, testDataset, scaleBy="z-score"){
	trainX			<- trainDataset$X
	testX			<- testDataset$X
	n				<- ncol(trainX)
	trainXColNames	<- colnames(trainX)

	if ( scaleBy == "min-max" || scaleBy == "minmax" ){
		trainColsRange	<- apply(trainX, 2, FUN=function(r) max(r) - min(r) )
		trainColsMin	<- apply(trainX, 2, FUN=min)

		scaledTrainX	<- sapply(1:n, function(col) (trainX[,col] - trainColsMin[col])/trainColsRange[col] )
		scaledTestX		<- sapply(1:n, function(col) (testX[,col]  - trainColsMin[col])/trainColsRange[col] )
		scaleInfo		<- list( Min = trainColsMin, Range = trainColsRange )

	}else if ( scaleBy == "z-score" || scaleBy == "zscore" ){
		trainColsSd		<- apply(trainX, 2, FUN=sd) 
		trainColsMean	<- apply(trainX, 2, FUN=mean)

		scaledTrainX	<- sapply(1:n, function(col) (trainX[,col] - trainColsMean[col])/trainColsSd[col] )
		scaledTestX		<- sapply(1:n, function(col) (testX[,col]  - trainColsMean[col])/trainColsSd[col] )
		scaleInfo		<- list( Sd = trainColsSd, Mean = trainColsMean )

	}else if ( scaleBy == "binarize" || scaleBy == "bin" ){
		scaledTrainX	<- BinarizeDataFrame(trainX)
		scaledTestX		<- BinarizeDataFrame(testX)
		scaleInfo		<- list()

	}else{
		scaledTrainX	<- trainX
		scaledTestX		<- testX
		scaleInfo		<- list()
	}
	
	colnames(scaledTrainX)	<- trainXColNames
	colnames(scaledTestX)	<- trainXColNames
	
	outputTrain		<- list( X = scaledTrainX,	Y = trainDataset$Y)
	outputTest		<- list( X = scaledTestX, 	Y = testDataset$Y )

	return ( list(Train = outputTrain, Test = outputTest, ScaleInfo = scaleInfo  ) )
}

BinarizeDataFrame <- function(df){
	return ( as.data.frame( (df>0)+0 ) )
}

DataFrameToFactor <- function(df){
	return( data.frame(lapply(df, as.factor)) )
}


## Calculates confusion table for given model and given test data
PredictAndMakeConfussionMatrix <- function(model, testData, predTransformFunction=NULL){
	yHat <- predict(model, testData$X)

	if ("class" %in% names( yHat )){
		yHat <- yHat$class
	}

	if (!is.null(predTransformFunction)){
		yHat <- predTransformFunction(yHat)
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
EvaluateModel <- function(model, testData, predTransformFunction=NULL){
	cm = PredictAndMakeConfussionMatrix(model, testData, predTransformFunction)
	return (
		list(
			Confusionmatrix	= cm,
			Statistics		= CalculateStatisticsForConfusionMatrix(cm)
		)
	)
}

EvaluateModelAndPlot <- function(model, trainData, testData, predTransformFunction=NULL, newWindow=T){
	if (newWindow) x11()

	resultTrain <- EvaluateModel(model, trainData, predTransformFunction)
	resultTest	<- EvaluateModel(model, testData,  predTransformFunction)

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

## Returns top N variables used for className by Naive Bayes model.
InspectNaiveBayes <- function(trainedNaiveBayes, className, topDecisiveCount=20){
	output				<- data.frame(stringsAsFactors=FALSE)
	variableNames		<- names(trainedNaiveBayes$tables)
	
	for(i in 1:length(trainedNaiveBayes$tables)){
		toAdd <- NULL
		value <- trainedNaiveBayes$tables[[i]]
		
		if ("1" %in% colnames(value)) {
			x <- value[,"1"][className]
			if (x != 0){
				output <- rbind( output, cbind(variableNames[i], x) )
			}
		}
	}

	rownames(output)	<- NULL
	colnames(output)	<- c("Variable", "P")
	topDecisiveCount	<- min(topDecisiveCount, nrow(output))
	output 				<- output[order(as.numeric( output$P )), ]

	return(output[1:topDecisiveCount, ])
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
stop_quietly();

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
modelSVM	<- svm(datasets$Train$X, datasets$Train$Y, kernel="radial")

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

datasets	<- PrepareTrainAndTest(authors, targetColumnName="Author", shuffle=T, trainToTestRatio=3/4, scaleBy="none")
train		<- datasets$Train
test		<- datasets$Test

modelNB		<- naiveBayes(train$X, train$Y)
EvaluateModelAndPlot(modelNB, train, test)

modelSVM	<- svm(train$X, train$Y, kernel="linear")
EvaluateModelAndPlot(modelSVM, train, test)

#  Inspect Naive Bayes:
InspectNaiveBayes(modelNB, "Foglar", 20)	
InspectNaiveBayes(modelNB, "Asimov", 20)	

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
