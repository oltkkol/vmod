## DEPDENCY #######################################################################################
source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_text.r", encoding="UTF-8")
source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_ml.r", encoding="UTF-8")

##	EXAMPLE 1
##	Bag Of Words vs language vs Authorship attribution
##	Naive approach, TF-IDF

asimovFiles			<- GetFilesContentsFromFolder("G:/VMOD/DATASETY/AsimovVSFoglar/Asimov", "ASIMOV")
foglarFiles			<- GetFilesContentsFromFolder("G:/VMOD/DATASETY/AsimovVSFoglar/Foglar", "FOGLAR")

asimovFileTokens	<- TokenizeTexts(asimovFiles)
foglarFileTokens	<- TokenizeTexts(foglarFiles)

asimovFileTokens	<- LimitTokensInTexts(asimovFileTokens, count=10)
foglarFileTokens	<- LimitTokensInTexts(foglarFileTokens, count=10)

allTokens		<- append(asimovFileTokens, foglarFileTokens)
allBOW			<- MakeBOWModel(allTokens)

sprintf("BOW has %d words", ncol(allBOW))

## --	Step 1: Naive Approach	-------------------------------------------------------------------
allBOW.Target	<- FirstColNameWordsToColumn(allBOW, "AuthorTarget")

datasets        <- PrepareTrainAndTest(allBOW.Target, "AuthorTarget", 3/4, scaleBy="none")
train           <- datasets$Train
test            <- datasets$Test

modelSVM		<- svm(train$X, train$Y, kernel='linear')
modelNB         <- naiveBayes(train$X, train$Y)

EvaluateModelAndPlot(modelSVM, train, test)
EvaluateModelAndPlot(modelNB, train, test)

#  Inspect Naive Bayes:
InspectNaiveBayes(modelNB, "FOGLAR", 20)	## troubles...
InspectNaiveBayes(modelNB, "ASIMOV", 20)

# Inspect spatiality
plot(cmdscale(dist(allBOW)), col=as.numeric(allBOW.Target$AuthorTarget))

## -- Step 2:

## --	Step 3: Better Approach	-------------------------------------------------------------------
## Build two corpora: Asimov and Foglar, use TF-IDF to remove common words in BOW

asimovCorpora   	<- MergeTexts(asimovFiles)
foglarCorpora   	<- MergeTexts(foglarFiles)

asimovTokens    	<- TokenizeText(asimovCorpora)
foglarTokens    	<- TokenizeText(foglarCorpora)

bowAsimovVsFoglar   <- MakeBOWModel( list(Asimov = asimovTokens, Foglar = foglarTokens) )
weights             <- CalculateTFIDFOnBOW(bowAsimovVsFoglar)

allTFIDFBOW			<- ApplyTFIDF(allBOW, weights)
sprintf("Original BOW has: %d, filtered by TF-IDF has: %d", ncol(allBOW), ncol(allTFIDFBOW))

allBOWTFIDF.Target	<- FirstColNameWordsToColumn(allTFIDFBOW, "AuthorTarget")

datasets        <- PrepareTrainAndTest(allBOWTFIDF.Target, "AuthorTarget", 3/4, scaleBy="none")
train           <- datasets$Train
test            <- datasets$Test

modelSVM		<- svm(train$X, train$Y, kernel='linear')
modelNB         <- naiveBayes(train$X, train$Y)

EvaluateModelAndPlot(modelSVM, train, test)
EvaluateModelAndPlot(modelNB, train, test)

#  Inspect Naive Bayes:
InspectNaiveBayes(modelNB, "FOGLAR", 20)
InspectNaiveBayes(modelNB, "ASIMOV", 20)