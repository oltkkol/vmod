## DEPDENCY #######################################################################################
source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_text.r", encoding="UTF-8")
source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_ml.r", encoding="UTF-8")

##
## Example 1 -- Bag Of Words vs language vs Authorship attribution

asimovFiles     <- GetFilesContentsFromFolder("G:/VMOD/DATASETY/AsimovVSFoglar/Asimov", "ASIMOV")
foglarFiles     <- GetFilesContentsFromFolder("G:/VMOD/DATASETY/AsimovVSFoglar/Foglar", "FOGLAR")

allFiles		<- append(asimovFiles, foglarFiles)
allTokens		<- TokenizeTexts(allFiles)
allTokens		<- LimitTokensInTexts(allTokens, count=500, takeRandom=TRUE, onlyOnce=T)
allBOW			<- MakeBOWModel(allTokens)

sprintf("BOW has %d words", ncol(allBOW))

allBOW.Target	<- FirstColNameWordsToColumn(allBOW, "AuthorTarget")

#  Build Train/Test datasets
datasets        <- PrepareTrainAndTest(allBOW.Target, "AuthorTarget", 3/4, scaleBy="none")
train           <- datasets$Train
test            <- datasets$Test

modelSVM		    <- svm(train$X, train$Y, kernel='linear')
modelNB             <- naiveBayes(train$X, train$Y)

EvaluateModelAndPlot(modelSVM, train, test)
EvaluateModelAndPlot(modelNB, train, test)

#  Inspect Naive Bayes:
words	<- as.data.frame(   t( sapply( modelNB$tables, function(x) x[,1])  ) )
words[ order(-words$FOGLAR),][1:20,]		# top 10 decisive words for Foglar
words[ order(-words$ASIMOV),][1:20,]		# top 10 decisive words for Asimov

##
## Example 2 -- Bag Of Words vs language vs Authorship attribution 2
## Goal: Use TF-IDF for removing words common for both Asimov and Foglar

asimovCorpora   	<- MergeTexts(asimovFiles)
foglarCorpora   	<- MergeTexts(foglarFiles)

asimovTokens    	<- LimitTokens( TokenizeText(asimovCorpora), count=100)
foglarTokens    	<- LimitTokens( TokenizeText(foglarCorpora), count=100)

bowAsimovVsFoglar   <- MakeBOWModel( list(Asimov = asimovTokens, Foglar = foglarTokens) )
weights             <- CalculateTFIDFOnBOW(bowAsimovVsFoglar)

bowTFIDFied         <- ApplyTFIDF(bowAsimovVsFoglar, weights)

sprintf("Original BOW has: %d, filtered by TF-IDF has: %d", ncol(bowAsimovVsFoglar), ncol(bowTFIDFied))

