## DEPDENCY #######################################################################################
source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_text.r", encoding="UTF-8")
source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_ml.r", encoding="UTF-8")

## Example 1 -- Bag Of Words vs language vs Authorship attribution

#  1. Read both authors files
asimovFiles     <- GetFilesContentsFromFolder("G:/VMOD/DATASETY/AsimovVSFoglar/Asimov", "ASIMOV")
foglarFiles     <- GetFilesContentsFromFolder("G:/VMOD/DATASETY/AsimovVSFoglar/Foglar", "FOGLAR")

#  2. Process texts & make bag-of-words
allFiles        <- append(asimovFiles, foglarFiles)
allTokens       <- TokenizeTexts(allFiles)
allTokens       <- LimitTokensInTexts(allTokens, count=100, takeRandom=TRUE)
allBOW          <- MakeBOWModel(allTokens)

#  3. Set AuthorTarget as class by prepended authors name
allBOWDataset   <- FirstColNameWordsToColumn(allBOW, "AuthorTarget")

#  4. Build Train/Test datasets
datasets        <- PrepareTrainAndTest(allBOWDataset, "AuthorTarget", 3/4, scaleBy="none")
train           <- datasets$Train
test            <- datasets$Test

#  5. Train models
modelSVM		    <- svm(train$X, train$Y, kernel='linear')
modelNB             <- naiveBayes(train$X, train$Y)

EvaluateModelAndPlot(modelSVM, train, test)
EvaluateModelAndPlot(modelNB, train, test)

#  Inspect Naive Bayes:
words	<- as.data.frame(   t( sapply( modelNB$tables, function(x) x[,1])  ) )
words[ order(-words$FOGLAR),][1:20,]		# top 10 decisive words for Foglar
words[ order(-words$ASIMOV),][1:20,]		# top 10 decisive words for Asimov




asimovCorpora   <- MergeTexts(asimovFiles)
foglarCorpora   <- MergeTexts(foglarFiles)

asimovTokens    <- TokenizeText(asimovCorpora)
foglarTokens    <- TokenizeText(foglarCorpora)

bowAsimovVsFoglar   <- MakeBOWModel( list(Asimov = asimovTokens, Foglar = foglarTokens) )
weights             <- CalculateTFIDFOnBOW(bowAsimovVsFoglar)

bowTFIDFied         <- ApplyTFIDF(bowAsimovVsFoglar, weights)
