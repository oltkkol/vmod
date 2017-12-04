## DEPDENCY #######################################################################################

source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_text.r", encoding="UTF-8")
source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_ml.r", encoding="UTF-8")

####################################################################################################
##  EXMAPLE 0
##  Simple examle with Bag Of Words
####################################################################################################

#  1. Read files & make Bag-Of-Words
adamFiles		<- GetFilesContentsFromFolder("D:/VMOD/DATASETY/Adam", "Adam")
henryFiles		<- GetFilesContentsFromFolder("D:/VMOD/DATASETY/HenryVegetarian", "HenryVegetarian")

adamTokens		<- TokenizeTexts(adamFiles)
henryTokens		<- TokenizeTexts(henryFiles)
allTokens		<- append(adamTokens, henryTokens)

allBOW			<- MakeBOWModel(allTokens)

#  2. Prepare datasets
allBOW			<- FirstColNameWordsToColumn(allBOW)
datasets		<- PrepareTrainAndTest(allBOW, "CLASS", 1/2, scaleBy="none", convertToFactors=TRUE)

#  3. Train Naive Bayes
modelNB			<- naiveBayes(datasets$Train$X, datasets$Train$Y)
EvaluateModelAndPlot(modelNB, datasets$Train, datasets$Test)

#  4. See inside Naive Bayes
InspectNaiveBayes(modelNB, "Adam")
InspectNaiveBayes(modelNB, "HenryVegetarian")

####################################################################################################
##	EXAMPLE 1
##	Bag Of Words vs language vs Authorship attribution
##	Naive approach 
####################################################################################################

#  1. Read files & make Bag-Of-Words
asimovFiles			<- GetFilesContentsFromFolder("D:/VMOD/DATASETY/Asimov", "ASIMOV")
foglarFiles			<- GetFilesContentsFromFolder("D:/VMOD/DATASETY/Foglar", "FOGLAR")

asimovFileTokensAll	<- TokenizeTexts(asimovFiles)
foglarFileTokensAll	<- TokenizeTexts(foglarFiles)

numberOfTokens		<- 50
asimovFileTokens	<- LimitTokensInTexts(asimovFileTokensAll, count=numberOfTokens, takeRandom=TRUE)
foglarFileTokens	<- LimitTokensInTexts(foglarFileTokensAll, count=numberOfTokens, takeRandom=TRUE)

allTokens		<- append(asimovFileTokens, foglarFileTokens)
allBOW			<- MakeBOWModel(allTokens)
allBOW.Target   <- FirstColNameWordsToColumn(allBOW,  "AuthorTarget")

sprintf("Original BOW has %d words", ncol(allBOW))

#  2. Train Naive Bayes with binary features (YES/NO; see convertToFactors=TRUE)
datasets	<- PrepareTrainAndTest(allBOW.Target, "AuthorTarget", 2/3, scaleBy="binarize", convertToFactors=TRUE)
modelBayes	<- naiveBayes(datasets$Train$X, datasets$Train$Y)

EvaluateModelAndPlot(modelBayes, datasets$Train, datasets$Test)

#  3. Train SVM with binary features (numeric 1/0; see convertToFactors=FALSE)
datasets	<- PrepareTrainAndTest(allBOW.Target, "AuthorTarget", 2/3, scaleBy="binarize", convertToFactors=FALSE)
modelSVM    <- svm(datasets$Train$X, datasets$Train$Y) 

EvaluateModelAndPlot(modelSVM, datasets$Train, datasets$Test)

####################################################################################################
##	EXAMPLE 2
##	Bag Of Words vs language vs Authorship attribution
##	Good approach
####################################################################################################

#  1. Read files & Tokenize them (+ limit to random X words)
asimovFiles			<- GetFilesContentsFromFolder("D:/VMOD/DATASETY/Asimov", "ASIMOV")
foglarFiles			<- GetFilesContentsFromFolder("D:/VMOD/DATASETY/Foglar", "FOGLAR")

asimovFileTokensAll	<- TokenizeTexts(asimovFiles)
foglarFileTokensAll	<- TokenizeTexts(foglarFiles)

numberOfTokens		<- 50
asimovFileTokensAll	<- LimitTokensInTexts(asimovFileTokensAll, count=numberOfTokens, takeRandom=TRUE)
foglarFileTokensAll	<- LimitTokensInTexts(foglarFileTokensAll, count=numberOfTokens, takeRandom=TRUE)

#  2. Bag of Words: per texts
allBOW				<- MakeBOWModel( append( asimovFileTokensAll, foglarFileTokensAll) )

#  3. Bag Of Words: per author
asimovCorpora   	<- MergeTokenizedTexts(asimovFileTokensAll)
foglarCorpora   	<- MergeTokenizedTexts(foglarFileTokensAll)

bowAsimovVsFoglar   <- MakeBOWModel( list(Asimov = asimovCorpora, Foglar = foglarCorpora) )

#  - calculate per author TF-IDF to identify author specific words:
weights             <- CalculateTFIDFOnBOW(bowAsimovVsFoglar, omitZeroWeightTerms=TRUE)
keepingWords		<- names(weights)
newAllBOW			<- KeepOnlyGivenColumns(allBOW, keepingWords)

sprintf("All BOW has: %d words. New authors specific BOW has: %d words", ncol(bowAsimovVsFoglar), ncol(newAllBOW))

#  5. Prepare datasets & Train
newAllBOW.Target	<- FirstColNameWordsToColumn(newAllBOW,  "AuthorTarget")

#  - see: binarize, factors=TRUE
datasets	<- PrepareTrainAndTest(newAllBOW.Target, "AuthorTarget", 2/3, scaleBy="binarize", convertToFactors=TRUE)
modelNB		<- naiveBayes(datasets$Train$X, datasets$Train$Y)
EvaluateModelAndPlot(modelNB, datasets$Train, datasets$Test)

InspectNaiveBayes(modelNB, "FOGLAR", 20)
InspectNaiveBayes(modelNB, "ASIMOV", 20)

#  - see: binarize, factors=FALSE
datasets	<- PrepareTrainAndTest(newAllBOW.Target, "AuthorTarget", 2/3, scaleBy="binarize", convertToFactors=FALSE)
modelSVM	<- svm(datasets$Train$X, datasets$Train$Y, kernel='linear')
EvaluateModelAndPlot(modelSVM, datasets$Train, datasets$Test)

####################################################################################################
##	EXAMPLE 3
##	Bag Of Words & Naive Bays: Sentiment Analysis
####################################################################################################

goodFiles		<- GetFilesContentsFromFolder("C:/DATA/Sentiment/GOOD", "GOOD") # 1000 files with positive reviews
badFiles		<- GetFilesContentsFromFolder("C:/DATA/Sentiment/BAD",  "BAD")  # 1000 files with negative reviews

goodTokens		<- TokenizeTexts(goodFiles)
badTokens		<- TokenizeTexts(badFiles)

# BOW per texts
allBOW			<- MakeBOWModel( append( goodTokens, badTokens) )

# BOW per sentiment
goodCorpora   	<- MergeTokenizedTexts(goodTokens)
badCorpora   	<- MergeTokenizedTexts(badTokens)
bowGoodVsBad	<- MakeBOWModel( list(Goods = goodCorpora, Bads = badCorpora) )

# Remove unsignificant words
weights         <- CalculateTFIDFOnBOW(bowGoodVsBad, omitZeroWeightTerms=TRUE)
newAllBOW		<- KeepOnlyGivenColumns(allBOW, names(weights))

# Prepare dataset & Train & Eval
newAllBOW.Target	<- FirstColNameWordsToColumn(newAllBOW,  "AuthorTarget")

datasets	<- PrepareTrainAndTest(newAllBOW.Target, "AuthorTarget", 2/3, scaleBy="binarize", convertToFactors=TRUE)
modelNB		<- naiveBayes(datasets$Train$X, datasets$Train$Y)

EvaluateModelAndPlot(modelNB, datasets$Train, datasets$Test)
InspectNaiveBayes(modelNB, "GOOD", 20)
InspectNaiveBayes(modelNB, "BAD", 20)