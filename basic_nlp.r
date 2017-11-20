## DEPDENCY #######################################################################################

source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_text.r", encoding="UTF-8")
source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_ml.r", encoding="UTF-8")

####################################################################################################

##  EXMAPLE 0
##  Simple examle

adamFiles		<- GetFilesContentsFromFolder("L:/VMOD/DATASETY/Simple/Adam", "Adam")
henryFiles		<- GetFilesContentsFromFolder("L:/VMOD/DATASETY/Simple/HenryVegetarian", "HenryVegetarian")

#  - inspect files
adamFiles
henryFiles

#  - tokenize
adamTokens		<- TokenizeTexts(adamFiles)
henryTokens		<- TokenizeTexts(henryFiles)
allTokens		<- append(adamTokens, henryTokens)

#  - build BOW
allBOW			<- MakeBOWModel(allTokens)
allBOW

#  - prepare for training
allBOW			<- FirstColNameWordsToColumn(allBOW)
datasets		<- PrepareTrainAndTest(allBOW, "CLASS", 1/2, scaleBy="none", convertToFactors=T)

modelNB			<- naiveBayes(datasets$Train$X, datasets$Train$Y)
EvaluateModelAndPlot(modelNB, datasets$Train, datasets$Test)

#  - inspect model
InspectNaiveBayes(modelNB, "Adam")
InspectNaiveBayes(modelNB, "HenryVegetarian")

##	EXAMPLE 1
##	Bag Of Words vs language vs Authorship attribution
##	Naive approach, TF-IDF

asimovFiles			<- GetFilesContentsFromFolder("G:/VMOD/DATASETY/AsimovVSFoglar/Asimov", "ASIMOV")
foglarFiles			<- GetFilesContentsFromFolder("G:/VMOD/DATASETY/AsimovVSFoglar/Lem", "FOGLAR")

asimovFileTokensAll	<- TokenizeTexts(asimovFiles)
foglarFileTokensAll	<- TokenizeTexts(foglarFiles)

numberOfTokens		<- 1000
asimovFileTokens	<- LimitTokensInTexts(asimovFileTokensAll, count=numberOfTokens, takeRandom=TRUE)
foglarFileTokens	<- LimitTokensInTexts(foglarFileTokensAll, count=numberOfTokens, takeRandom=TRUE)

allTokens		<- append(asimovFileTokens, foglarFileTokens)
allBOW			<- MakeBOWModel(allTokens)
allBOW			<- BinarizeDataFrame(allBOW)

sprintf("BOW has %d words", ncol(allBOW))

## --	Step 1: Naive Approach	-------------------------------------------------------------------
allBOW.Target	<- FirstColNameWordsToColumn(allBOW, "AuthorTarget")

datasets        <- PrepareTrainAndTest(allBOW.Target, "AuthorTarget", 2/3, scaleBy="none")
train           <- datasets$Train
test            <- datasets$Test

train$X			<- as.data.frame(train$X)
test$X			<- as.data.frame(test$X)

modelSVM		<- svm(train$X, train$Y, kernel='linear')
modelNB         <- naiveBayes(train$X, train$Y)

EvaluateModelAndPlot(modelSVM, train, test)
EvaluateModelAndPlot(modelNB, train, test)


# Inspect Naive Bayes:
InspectNaiveBayes(modelNB, "FOGLAR", 20)	
InspectNaiveBayes(modelNB, "ASIMOV", 20)

# Inspect spatiality for SVM
plot(cmdscale(dist(allBOW)), col=as.numeric(as.factor(allBOW.Target$AuthorTarget)))

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