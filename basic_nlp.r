## DEPDENCY #######################################################################################

source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_text.r", encoding="UTF-8")
source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_ml.r", encoding="UTF-8")

####################################################################################################

##  EXMAPLE 0
##  Simple examle

#  - read data and prepare BOW
adamFiles		<- GetFilesContentsFromFolder("L:/VMOD/DATASETY/Simple/Adam", "Adam")
henryFiles		<- GetFilesContentsFromFolder("L:/VMOD/DATASETY/Simple/HenryVegetarian", "HenryVegetarian")

adamTokens		<- TokenizeTexts(adamFiles)
henryTokens		<- TokenizeTexts(henryFiles)
allTokens		<- append(adamTokens, henryTokens)

allBOW			<- MakeBOWModel(allTokens)

#  - prepare for training, train
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

asimovFiles			<- GetFilesContentsFromFolder("L:/VMOD/DATASETY/AsimovVSFoglar/Asimov", "ASIMOV")
foglarFiles			<- GetFilesContentsFromFolder("L:/VMOD/DATASETY/AsimovVSFoglar/Foglar", "FOGLAR")

asimovFileTokensAll	<- TokenizeTexts(asimovFiles)
foglarFileTokensAll	<- TokenizeTexts(foglarFiles)

numberOfTokens		<- 1000
asimovFileTokens	<- LimitTokensInTexts(asimovFileTokensAll, count=numberOfTokens, takeRandom=TRUE)
foglarFileTokens	<- LimitTokensInTexts(foglarFileTokensAll, count=numberOfTokens, takeRandom=TRUE)

allTokens		<- append(asimovFileTokens, foglarFileTokens)
allBOW			<- MakeBOWModel(allTokens)

sprintf("Original BOW has %d words", ncol(allBOW))

## --	Step 1: Naive Approach	-------------------------------------------------------------------
allBOW.Target   <- FirstColNameWordsToColumn(allBOW, "AuthorTarget")

#  - train on binarized data (has word / hasn't word; see convertToFactors=TRUE & scaleBy="binarize")
datasets    <- PrepareTrainAndTest(allBOW.Target, "AuthorTarget", 2/3, scaleBy="binarize", convertToFactors=TRUE)
m			<- naiveBayes(datasets$Train$X, datasets$Train$Y)

EvaluateModelAndPlot(m, datasets$Train, datasets$Test)

#  - train on word frequencies (see convertToFactors=FALSE & scaleBy="none")
datasets	<- PrepareTrainAndTest(allBOW.Target, "AuthorTarget", 2/3, scaleBy="none", convertToFactors=FALSE)
m			<- svm(datasets$Train$X, datasets$Train$Y)
n			<- naiveBayes(datasets$Train$X, datasets$Train$Y, laplace=1)

EvaluateModelAndPlot(m, datasets$Train, datasets$Test)
EvaluateModelAndPlot(n, datasets$Train, datasets$Test)

sprintf("... training BOW has %d words", ncol(datasets$Train$X))

## --	Step 2: TFIDF	-------------------------------------------------------------------
## Build two corpora: Asimov and Foglar, use TF-IDF to remove common words in BOW

asimovCorpora   	<- MergeTexts(asimovFiles)
foglarCorpora   	<- MergeTexts(foglarFiles)

asimovTokens    	<- TokenizeText(asimovCorpora)
foglarTokens    	<- TokenizeText(foglarCorpora)

bowAsimovVsFoglar   <- MakeBOWModel( list(Asimov = asimovTokens, Foglar = foglarTokens) )
weights             <- CalculateTFIDFOnBOW(bowAsimovVsFoglar)
keepingWords		<- names(weights)

allBOWFiltered		<- KeepOnlyGivenColumns(allBOW, keepingWords)

#  - prepare datasets
allBOWFiltered.Target	<- FirstColNameWordsToColumn(allBOWFiltered, "AuthorTarget")

datasets        <- PrepareTrainAndTest(allBOWFiltered.Target, "AuthorTarget", 2/3, scaleBy="binarize")
train           <- datasets$Train
test            <- datasets$Test

modelSVM		<- svm(train$X, train$Y, kernel='linear')
modelNB         <- naiveBayes(train$X, train$Y)

EvaluateModelAndPlot(modelSVM, train, test)
EvaluateModelAndPlot(modelNB, train, test)

#  Inspect Naive Bayes:
#InspectNaiveBayes(modelNB, "FOGLAR", 20)
#InspectNaiveBayes(modelNB, "ASIMOV", 20)

# Some stats
sprintf("Original BOW has: %d words, filtered by TF-IDF has: %d words. After removing zero variance: %d words", ncol(allBOW), ncol(allBOWFiltered), ncol(train$X))