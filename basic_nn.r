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

	plot(env$train.acc.log, type="o", col="blue")   # :-/
    points(env$test.acc.log, type="o", col="red")
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

#  1. Prepare data (see PrepareDatasetsForNeuralNetwork converting factors to numeric & DF to data.matrix)
data(Sonar)

datasets	<- PrepareTrainAndTest(Sonar, "Class", 2/3, scaleBy="z-score")
datasets	<- PrepareDatasetsForNeuralNetwork(datasets)

#  2. Prepare NN Model:
data <- mx.symbol.Variable("data")

# 20 -> 20 -> 20 -> 20 -> 2 network with relus
A   <- mx.symbol.FullyConnected(data=data, num_hidden=20)
A_o <- mx.symbol.Activation(data=A, act_type="relu") 

B   <- mx.symbol.FullyConnected(data=A_o, num_hidden=20)
B_o <- mx.symbol.Activation(data=B, act_type="relu") 

C   <- mx.symbol.FullyConnected(data=B_o, num_hidden=20)
C_o <- mx.symbol.Activation(data=C, act_type="relu") 

D   <- mx.symbol.FullyConnected(data=C_o, num_hidden=20)
D_o <- mx.symbol.Activation(data=D, act_type="relu") 

E   <- mx.symbol.FullyConnected(data=D_o, num_hidden=2)
E_o <- mx.symbol.SoftmaxOutput(E)

#  3. Create & Train:
model <- mx.model.FeedForward.create( E_o,
                                        X = datasets$Train$X, 
                                        y = datasets$Train$Y, 
                                        eval.data=list(data=datasets$Test$X, label=datasets$Test$Y),
                                        optimizer="adam",
                                        ctx=mx.cpu(),     
                                        learning.rate=0.0001, 
                                        wd=0.000183,
                                        num.round=200, 
										array.batch.size=10,
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
authorA_Files	<- GetFilesContentsFromFolder("D:/VMOD/DATASETY/Asimov", "ASIMOV")
authorB_Files	<- GetFilesContentsFromFolder("D:/VMOD/DATASETY/Foglar", "FOGLAR")

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

data <- mx.symbol.Variable("data")

A   <- mx.symbol.FullyConnected(data=data, num_hidden=100)
A_o <- mx.symbol.Activation(data=A, act_type="relu") 

B   <- mx.symbol.FullyConnected(data=A_o, num_hidden=10)
B_o <- mx.symbol.Activation(data=B, act_type="relu") 

C   <- mx.symbol.FullyConnected(data=B_o, num_hidden=2)
C_o <- mx.symbol.SoftmaxOutput(C)

mx.set.seed(0)
model <- mx.model.FeedForward.create(C_o,
                                        X = datasets$Train$X, 
                                        y = datasets$Train$Y, 
                                        eval.data=list(data=datasets$Test$X, label=datasets$Test$Y),
                                        optimizer="adam",
                                        ctx=mx.cpu(),     
                                        num.round=50, 
                                        learning.rate=0.00001, 
										array.batch.size=2,
                                        wd=0.0001,
										eval.metric=mx.metric.accuracy,
                                        epoch.end.callback=plotMetric)

EvaluateModelAndPlot(model, datasets$Train, datasets$Test, GetSoftmaxResult)
