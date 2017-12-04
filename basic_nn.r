## DEPDENCY #######################################################################################

source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_text.r", encoding="UTF-8")
source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_ml.r", encoding="UTF-8")

rlibrary("mxnet")

plotMetric <- function(a, b, env, ...){ 
	if (is.null(env$acc.log)) env$acc.log <- c()
	env$acc.log <- append(env$acc.log, env$metric$get(env$train.metric)$value)

	plot(env$acc.log) 
	return (TRUE)
}

GetSoftmaxResult <- function(softmaxOutput){
	return ( max.col( t(softmaxOutput) ) -1 )
}

####################################################################################################
##	EXAMPLE 1: Sonar
####################################################################################################

data(Sonar)
datasets			<- PrepareTrainAndTest(Sonar, "Class")
datasets$Train$X	<- data.matrix(datasets$Train$X)
datasets$Test$X		<- data.matrix(datasets$Test$X)
datasets$Train$Y	<- as.numeric(datasets$Train$Y)-1
datasets$Test$Y		<- as.numeric(datasets$Test$Y)-1

datasets$Train$Y
datasets$Test$Y


data <- mx.symbol.Variable("data")

A   <- mx.symbol.FullyConnected(data=data, num_hidden=200)
A_o <- mx.symbol.Activation(data=A, act_type="relu") 

B   <- mx.symbol.FullyConnected(data=A_o, num_hidden=10)
B_o <- mx.symbol.Activation(data=B, act_type="relu") 

C   <- mx.symbol.FullyConnected(data=B_o, num_hidden=3)
C_o <- mx.symbol.SoftmaxOutput(C)

mx.set.seed(0)
model <- mx.model.FeedForward.create(C_o,
                                        X=datasets$Train$X, 
                                        y=datasets$Train$Y, 
                                        optimizer="adam",
                                        ctx=mx.cpu(),     
                                        num.round=200, 
                                        learning.rate=0.1, 
										array.batch.size=20,
                                        wd=0.01,
										eval.metric=mx.metric.accuracy,
                                        epoch.end.callback=plotMetric)

graph.viz(model$symbol)

EvaluateModelAndPlot(model, datasets$Train, datasets$Test, GetSoftmaxResult)
