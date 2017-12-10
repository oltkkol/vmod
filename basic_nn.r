## DEPDENCY #######################################################################################

source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_text.r", encoding="UTF-8")
source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_ml.r", encoding="UTF-8")

plotMetric <- function(a, b, env, ...){ 
	if (is.null(env$train.acc.log)){
		env$train.acc.log   <- c()
		env$test.acc.log    <- c()
	} 

	env$train.acc.log   <- append(env$train.acc.log, env$metric$get(env$train.metric)$value)
	env$test.acc.log    <- append(env$test.acc.log,  env$metric$get(env$eval.metric)$value)

	layout(matrix(c(1,1,1,1,2,2,2,2), nrow=2, byrow=T))
	plot(env$train.acc.log, type="o", col="blue")   # :-/
	points(env$test.acc.log, type="o", col="red")

	hist(as.matrix( env$model$arg.params[[1]] ), breaks=30)

	return (TRUE)
}

GetSoftmaxResult <- function(softmaxOutput){
	return ( max.col( t(softmaxOutput) ) -1 )
}

PrepareDatasetsForNeuralNetwork <- function(datasets){
	datasets$Train$X	<- data.matrix(datasets$Train$X)
	datasets$Test$X		<- data.matrix(datasets$Test$X)
	datasets$Train$Y	<- as.numeric(datasets$Train$Y) - 1
	datasets$Test$Y		<- as.numeric(datasets$Test$Y)  - 1 	

	return (datasets)
}

stop_quietly();

####################################################################################################
##	EXAMPLE 1: Sonar
####################################################################################################

#  1. Prepare data (see "PrepareDatasetsForNeuralNetwork" converting all to numeric and to data.matrix)
data(Sonar)

datasets	<- PrepareTrainAndTest(Sonar, "Class", 2/3, scaleBy="z-score")
datasets	<- PrepareDatasetsForNeuralNetwork(datasets)

#  2. Prepare NN Model: 20 -> 20 -> 20 -> 20 -> 2 Neural network with RELU activations
myNeuralNetwork <-(
	mx.symbol.Variable("data") %>%

	mx.symbol.FullyConnected(num_hidden = 20)	%>% 
	mx.symbol.Activation(act_type = "relu")		%>%

	mx.symbol.FullyConnected(num_hidden = 20)	%>%
	mx.symbol.Activation(act_type = "relu")		%>%

	mx.symbol.FullyConnected(num_hidden = 20)	%>%
	mx.symbol.Activation(act_type = "relu")		%>%

	mx.symbol.FullyConnected(num_hidden = 20)	%>%
	mx.symbol.Activation(act_type = "relu")		%>%

	mx.symbol.FullyConnected(num_hidden = 2)	%>%
	mx.symbol.SoftmaxOutput()
)

#  3. Create & Train:
model <- mx.model.FeedForward.create( myNeuralNetwork,
										X = datasets$Train$X, 
										y = datasets$Train$Y, 
										eval.data=list(data=datasets$Test$X, label=datasets$Test$Y),
										optimizer="adam",
										ctx=mx.cpu(),     
										num.round=200, 
										array.batch.size=10,
										learning.rate=0.0001, 
										wd=0.000183,
										eval.metric=mx.metric.accuracy,
										epoch.end.callback=plotMetric )

#  4. Display our network
graph.viz(model$symbol)

#  5. Evaluate
EvaluateModelAndPlot(model, datasets$Train, datasets$Test, GetSoftmaxResult)

#  6. Benchmark with LDA, SVM & naiveBayes
datasets	<- PrepareTrainAndTest(Sonar, "Class", 2/3, scaleBy="z-score")
#datasets	<- PrepareTrainAndTest(Sonar, "Class", 2/3, scaleBy="minmax")

model		<- lda(datasets$Train$X, datasets$Train$Y)
EvaluateModelAndPlot(model, datasets$Train, datasets$Test)

model		<- svm(datasets$Train$X, datasets$Train$Y, kernel="linear")
EvaluateModelAndPlot(model, datasets$Train, datasets$Test)

model		<- naiveBayes(datasets$Train$X, datasets$Train$Y)
EvaluateModelAndPlot(model, datasets$Train, datasets$Test)

####################################################################################################
##	EXAMPLE 2: Easy authorship with BOW
####################################################################################################

## [1] Prepare BOW

#  1. Read files & Tokenize them (+ limit to random X words)
authorA_Files	<- GetFilesContentsFromFolder("F:/VMOD/DATASETY/Asimov", "ASIMOV")
authorB_Files	<- GetFilesContentsFromFolder("F:/VMOD/DATASETY/Foglar", "FOGLAR")

authorA_Tokens	<- TokenizeTexts(authorA_Files)
authorB_Tokens	<- TokenizeTexts(authorB_Files)

numberOfTokens	<- 50
authorA_Tokens	<- LimitTokensInTexts(authorA_Tokens, count=numberOfTokens, takeRandom=TRUE)
authorB_Tokens	<- LimitTokensInTexts(authorB_Tokens, count=numberOfTokens, takeRandom=TRUE)

#  2. Bag of Words: per texts
allBOW				<- MakeBOWModel( append( authorA_Tokens, authorB_Tokens) )

#  3. Bag Of Words: per author
authorA_Corpora   	<- MergeTokenizedTexts(authorA_Tokens)
authorB_Corpora   	<- MergeTokenizedTexts(authorB_Tokens)
bowAuthorAVsB		<- MakeBOWModel( list(AuthorA = authorA_Corpora, AuthorB = authorB_Corpora) )

#  - calculate per author TF-IDF to identify author specific words:
weights             <- CalculateTFIDFOnBOW(bowAuthorAVsB, omitZeroWeightTerms=TRUE)
newAllBOW			<- KeepOnlyGivenColumns( allBOW, names(weights) )

#  5. Prepare datasets & Train
newAllBOW.Target	<- FirstColNameWordsToColumn(newAllBOW,  "AuthorTarget")

## [2] Prepare datasets
datasets	<- PrepareTrainAndTest(newAllBOW.Target, "AuthorTarget", 2/3, scaleBy="binarize", convertToFactors=FALSE)
datasets	<- PrepareDatasetsForNeuralNetwork(datasets)

## [3] Prepare & Train Neural Network:
myNN	<-(
	mx.symbol.Variable("data")					%>%
	mx.symbol.FullyConnected(num_hidden=100)	%>%
	mx.symbol.Activation(act_type="relu")		%>%

	mx.symbol.FullyConnected(num_hidden=10)		%>%
	mx.symbol.Activation(act_type="relu")		%>%

	mx.symbol.FullyConnected(num_hidden=2)		%>%
	mx.symbol.SoftmaxOutput()
)

mx.set.seed(0)
model <- mx.model.FeedForward.create(myNN,
										X = datasets$Train$X, 
										y = datasets$Train$Y, 
										eval.data=list(data=datasets$Test$X, label=datasets$Test$Y),
										optimizer="adam",
										ctx=mx.cpu(),     
										num.round=50, 
										array.batch.size=2,
										learning.rate=0.00001, 
										wd=0.0001,
										eval.metric=mx.metric.accuracy,
										epoch.end.callback=plotMetric)

EvaluateModelAndPlot(model, datasets$Train, datasets$Test, GetSoftmaxResult)


####################################################################################################
##	EXAMPLE 3: Sentiment BOW
####################################################################################################

goodTokens		<- TokenizeTexts( GetFilesContentsFromFolder("C:/DATA/NLP/Sentiment/GOOD", "GOOD") )    # 1000 files with positive reviews
badTokens		<- TokenizeTexts( GetFilesContentsFromFolder("C:/DATA/NLP/Sentiment/BAD",  "BAD")  )    # 1000 files with negative reviews

goodCorpora   	<- MergeTokenizedTexts(goodTokens)
badCorpora   	<- MergeTokenizedTexts(badTokens)

# BOW per Texts & per sentiment, apply TFIDF on sentiment
allBOW			<- MakeBOWModel( append( goodTokens, badTokens) )
bowGoodVsBad	<- MakeBOWModel( list(Goods = goodCorpora, Bads = badCorpora) )

weights         <- CalculateTFIDFOnBOW(bowGoodVsBad, omitZeroWeightTerms=TRUE)
newAllBOW		<- KeepOnlyGivenColumns(allBOW, names(weights))

# Prepare dataset & Train & Eval
newAllBOW.Target	<- FirstColNameWordsToColumn(newAllBOW,  "Sentiment")
datasets	        <- PrepareTrainAndTest(newAllBOW.Target, "Sentiment", 2/3, scaleBy="z-score")
datasets	        <- PrepareDatasetsForNeuralNetwork(datasets)

net	<-(	mx.symbol.Variable( "data" )					%>%
		mx.symbol.Dropout( p=0.3 )						%>%

		mx.symbol.FullyConnected( num_hidden=30 )		%>%
		mx.symbol.Dropout( p=0.1 )						%>%

		mx.symbol.Activation( act_type="relu" )			%>%
		mx.symbol.FullyConnected( num_hidden=5 )		%>%

		mx.symbol.Activation( act_type="relu" )			%>%
		mx.symbol.FullyConnected( num_hidden=2 )		%>%

		mx.symbol.SoftmaxOutput()
)

model <- mx.model.FeedForward.create(net,
										X			= datasets$Train$X,
										y			= datasets$Train$Y,
										eval.data	= list(data=datasets$Test$X, label=datasets$Test$Y),
										optimizer	= "adam",
										ctx			= mx.cpu(),     
										num.round		= 100, 
										array.batch.size= 250,
										learning.rate	= 0.001, 
										wd = 0.001,
										eval.metric = mx.metric.accuracy,
										epoch.end.callback = plotMetric)

EvaluateModelAndPlot(model, datasets$Train, datasets$Test, GetSoftmaxResult)