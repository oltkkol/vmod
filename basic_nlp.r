## DEPDENCY #######################################################################################
source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_text.r", encoding="UTF-8")
source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_ml.r", encoding="UTF-8")

##	EXAMPLE 1
##	Bag Of Words vs language vs Authorship attribution
##	Naive approach, TF-IDF

asimovFiles			<- GetFilesContentsFromFolder("G:/VMOD/DATASETY/AsimovVSFoglar/Asimov", "ASIMOV")
foglarFiles			<- GetFilesContentsFromFolder("G:/VMOD/DATASETY/AsimovVSFoglar/Verne", "FOGLAR")

asimovFileTokensAll	<- TokenizeTexts(asimovFiles)
foglarFileTokensAll	<- TokenizeTexts(foglarFiles)

numberOfTokens		<- 1000
asimovFileTokens	<- LimitTokensInTexts(asimovFileTokensAll, count=numberOfTokens, takeRandom=TRUE)
foglarFileTokens	<- LimitTokensInTexts(foglarFileTokensAll, count=numberOfTokens, takeRandom=TRUE)

allTokens		<- append(asimovFileTokens, foglarFileTokens)
allBOW			<- MakeBOWModel(allTokens)
allBOW			<- BinarizeMatrix(allBOW)

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

# Inspect Naive Bayes:
InspectNaiveBayes(modelNB, "FOGLAR", 20)	
InspectNaiveBayes(modelNB, "ASIMOV", 20)

# Inspect spatiality for SVM
plot(cmdscale(dist(allBOW)), col=as.numeric(allBOW.Target$AuthorTarget))

## -- Step 2: Naive TF-IDF ------------------------------------------------------------------


## --	Step 3: Better Approach	-------------------------------------------------------------------
## Build two corpora: Asimov and Foglar, use TF-IDF to remove common words in BOW

asimovCorpora   	<- MergeTexts(asimovFiles)
foglarCorpora   	<- MergeTexts(foglarFiles)

asimovTokens    	<- TokenizeText(asimovCorpora)
foglarTokens    	<- TokenizeText(foglarCorpora)

bowAsimovVsFoglar   <- MakeBOWModel( list(Asimov = asimovTokens, Foglar = foglarTokens) )
weights             <- CalculateTFIDFOnBOW(bowAsimovVsFoglar)
keepingWords		<- names(weights)

allBOWFiltered		<- KeepOnlyGivenColumns(allBOW, keepingWords)
allBOWFiltered		<- BinarizeMatrix(allBOWFiltered)

sprintf("Original BOW has: %d words, filtered by TF-IDF has: %d words", ncol(allBOW), ncol(allBOWFiltered))

allBOWFiltered.Target	<- FirstColNameWordsToColumn(allBOWFiltered, "AuthorTarget")

datasets        <- PrepareTrainAndTest(allBOWFiltered.Target, "AuthorTarget", 3/4, scaleBy="none")
train           <- datasets$Train
test            <- datasets$Test

modelSVM		<- svm(train$X, train$Y, kernel='linear')
modelNB         <- naiveBayes(train$X, train$Y)

EvaluateModelAndPlot(modelSVM, train, test)
EvaluateModelAndPlot(modelNB, train, test)

#  Inspect Naive Bayes:
InspectNaiveBayes(modelNB, "FOGLAR", 20)
InspectNaiveBayes(modelNB, "ASIMOV", 20)