## DEPENDENCY #####################################################################################
source("https://raw.githubusercontent.com/oltkkol/vmod/master/rlibrary_dependency.r", encoding="UTF-8")

## FILE WORK	###################################################################################

# reads content of file as plain text
GetFileContent <- function(fileName){
	readedText <- readLines(fileName, encoding="UTF-8")
	readedText <- do.call(paste, c(as.list(readedText), sep=" "))
	return(readedText)
}

# reads all files and their contents from given folder and saves them into named list
# Eg.: GetFilesContentsFromFolder("/Documents/Texts/")
# Eg.: GetFilesContentsFromFolder("/Documents/Asimov/", "ASIMOV")	#... prepends all names with ASIMOV
GetFilesContentsFromFolder <- function(folderPath, prependNameBy=NULL){
	getFileContentFromFolderFile <- function(fileName)	{
		fileName <- paste(folderPath, fileName, sep="/")
		return( GetFileContent(fileName) )
	}

	output <- list()
	for(file in list.files(folderPath)){
		output[file] <- getFileContentFromFolderFile(file)
	}

	if (length(output) > 0){
		if (is.null(prependNameBy) == FALSE){
			names(output) <- paste(prependNameBy, names(output))
		}
	}
	
	return (output);
}

# Merges vector of strings to one string
# Eg.: MergePlainTexts( c("hello", "there")  )
MergePlainTexts <- function(texts){
	return( do.call(paste, c(as.list(texts), sep=" ")) )
}
# Merges list of tokenized texts, returns one vector of all tokens
# Eg.: MergeTokenizedTexts( list(Text1 = c("Hello", "here", "is"), Text2 = c("Johny") ) )
MergeTokenizedTexts <- function(texts){
	return ( as.vector( unlist(texts) ) )
}

# adds a new column "CLASS" with first word from column names
# Eg.:	allBOW			<- MakeBOWModel(allTokens)
#		allBOWWithClass <- UseMatrixFirstColNameWordAsClassName(allBOW)
FirstColNameWordsToColumn <- function(dataMatrix, columnName="CLASS"){
    firstWords  <- str_match( rownames(dataMatrix) , "^\\w+")
    newMatrix   <- cbind(firstWords, dataMatrix)
    colnames(newMatrix)[1] <- columnName
    
    return(as.data.frame(newMatrix))
}

## TEXT WORK	###################################################################################

# Basic plain text tokenizer by regular expression split mask
# Eg.:	TokenizeText("Hey, this is an example")  returns "hey", "this", "is", "an", "example"
#		TokenizeText("Hey, this is an example", regexPattern="\\b\\w{3}\\b", regexIsMask=TRUE) returns "hey"
TokenizeText <- function(text, regexPattern="\\W+", regexIsMask=FALSE, convertToLowerCase=TRUE){
	if (regexIsMask){
		tokens = str_extract_all(text, regexPattern)
	}else{
		tokens = stri_split_regex(text, regexPattern)
	}

	tokens = unlist(tokens)

	if (convertToLowerCase){
		tokens = tolower(tokens)
	}

	tokens = tokens[ tokens != "" ]
	return(tokens)
}

# tokenizes list of texts
# Eg.: TokenizeTexts( c("Hey, this is an example.", "Yes. Good.") )
TokenizeTexts <- function(texts, ...){
	return( lapply(texts, function(t) TokenizeText(t, ...) ) )
}
 
# Gets types (dictionary) from token list
GetTypes <- function(tokensList){
	return( unique(tokensList) )
}

# Gets only given number of tokens; takeRandom=T takes N random tokens, onlyOnce=T each position only once
# Eg.: LimitTokens( c("well", "that's", "great", "i", "can", "take", "any"), count=3, takeRandom=T )
LimitTokens <- function(tokensList, count, takeRandom=F, onlyOnce=T){
	if (takeRandom == T){
        	return ( sample(tokensList, count, replace=!onlyOnce) )
    	}

	return ( tokensList[1:min(count, length(tokensList))] )
}

LimitTokensInTexts <- function(texts, count, ...){
	return ( lapply(texts, function(t) LimitTokens(t, count, ...)) )
}

## Deletes given list of tokens from tokens vector
## Eg.: RemoveTokens(c("yes", "good"), c("good"))
RemoveTokens <- function(tokens, removeTokens){
	for(toRemove in removeTokens){
		tokens <- RemoveToken(tokens, toRemove)
	}
	return (tokens)
}
			
## Read text file in vertical format: word form, lemma, pos
## Eg.: ReadVertical("foglar.lemma.txt")
ReadVertical <- function(file){
	plainText <- GetFileContent(file)
	vertical  <- TokenizeText(plainText, regexPattern="\\p{Z}", regexIsMask=F, convertToLowerCase=F)
	vertical  <- stri_split_regex(vertical, "\t")

	forms  <- sapply(vertical, function(e) e[[1]])
	lemmas <- sapply(vertical, function(e) e[[2]])
	poss   <- sapply(vertical, function(e) e[[3]])

	text   <- cbind(Word=forms,Lemma=lemmas, POS=poss)
	return (text)
}

			
## BOW MODEL	###################################################################################

RemoveAllZeroColumns <- function(dataset){
	return (dataset[, colSums(dataset != 0) > 0])
}

# Creates BOW model for list of tokenized texts
# Eg.: MakeBOWModel( list(text1 = c("John", "ate", "an", "apple"), text2 = c("Kate", "ate", "an", "orange") ) )
MakeBOWModel <- function(tokenizedTexts){
	allTokens		<- unlist( tokenizedTexts )
	globalDict		<- GetTypes( allTokens )

	dataMatrix				<- matrix(0, ncol = length(globalDict), nrow = length(names(tokenizedTexts)))
	colnames(dataMatrix)	<- globalDict
	rownames(dataMatrix)	<- names(tokenizedTexts)

	frequencyTables	<- lapply(tokenizedTexts, table)

	for(textName in names(tokenizedTexts)){
		f		<- frequencyTables[[textName]]
		types	<- names(f)

		dataMatrix[textName, types] <- f[types]
	}

	return ( as.data.frame( dataMatrix) )
}

# Gets TF-IDF weights for given BOW Matrix. Zero weighted terms are omitted!
# See example for ApplyTFIDF
CalculateTFIDFOnBOW <- function(bowMatrix, omitZeroWeightTerms=TRUE){
	bowMatrix	<- RemoveAllZeroColumns(bowMatrix)

	N			<- nrow(bowMatrix)
	Nt			<- apply( bowMatrix, 2, function(colData) sum(colData > 0) )
	weights		<- log(N / Nt)	
	
	if (omitZeroWeightTerms == TRUE){
		return ( weights[weights != 0] )
	}else{
		return ( weights )
	}
}
					 
# Makes n-grams from tokens.
# If Glue is a string, vector of n-gram strings are returned, otherwise a matrix
# Eg.: MakeNGrams( c("a", "b", "c", "d", "e", "f", "g"), n=3, glue=" ")
MakeNGrams <- function(tokens, n=2, glue="->"){
	m <- t( sapply( 1:(length(tokens)-n+1), function(i) tokens[i:(i+n-1)] ) )

	if (is.character(glue)){
		m <- apply(m, 1, function(r) paste(r, collapse=glue))
	}

	return( m )
}

## Makes list of distances betweet target token and target tokens list 
## Eg.: GetNearestTokensFromListToToken(c("x", "A", "x", "x", "B", "A", "x", "x", "C", "A"), "A", c("B", "C"))
##		returns list:	B = {3, 1} (as first encounter of A has nearest element B=3, second encounter has nearest B=1), 
##						C = {1}  (last encounter of A with nearest element C=1)

GetNearestTokensFromListToToken <- function(s, target, targetsList){
	posInfo <- list()

	for(i in 1:length(s)){
		if (s[i] == target){
			foundJ <- FALSE
			foundK <- FALSE

			for(j in i:1){
				if (s[j] %in% targetsList){
					foundJ = TRUE
					break
				}
			}

			for(k in i:length(s)){
				if (s[k] %in% targetsList){
					foundK = TRUE
					break
				} 
			}

			if (foundJ && foundK){
				if (i-j < k-i ){
					posInfo[[ s[j] ]] <- append( posInfo[[ s[j] ]], i-j )
				}else{
					posInfo[[ s[k] ]] <- append( posInfo[[ s[k] ]], k-i )
				}
			}else if(foundJ){
				posInfo[[ s[j] ]] <- append( posInfo[[ s[j] ]], i-j )
			}else if(foundK){
				posInfo[[ s[k] ]] <- append( posInfo[[ s[k] ]], k-i )
			}
		}
	}

	return(posInfo)
}	       

# Applies TF-IDF weighting to terms in bowMatrix.
# Eg.:	bowCorpora 	<- MakeBOWModel( list( text1=c("John", "ate", "an", "apple"), text2=c("Kate", "ate", "an", "orange") ) )
#		weights	 	<- CalculateTFIDFOnBOW(bowCorpora)
#		tfidfCorpora<- ApplyTFIDF(bowCorpora, weights)

ApplyTFIDF <- function(bowMatrix, weights){
	terms	<- as.vector( intersect( names(weights), colnames(bowMatrix) ) )
	result	<- bowMatrix[,terms, drop=FALSE] # omit any term that wasn't in original model dictionary

	result[,terms]	<- t( t( result[,terms]) * weights[terms] )

	return (result)
}

## STRING COMPARISON	###########################################################################

# Searches for the most similar word in a given string by sliding window and calculating DamerauLevensthein distance, returns pair of bestDistance and bestMatch
# Eg.: DamerauLevenstheinSliding(where = "the tree has dreamed", what = "hat") returns ("1", "has") as the most similar
DamerauLevenstheinSliding <- function(where, what){	
	bestDistance	<- Inf
	bestMatch		<- ""

	for(i in 1:nchar(where)) {
		testingString	<- substr(where, i, i+nchar(what)-1)
		distance		<- stringdist(what, testingString, method="dl")

		if (distance < bestDistance){
			bestDistance	<- distance
			bestMatch		<- testingString
		}
	}

	return ( c(bestDistance, bestMatch) )
}

# Searches the most similar word in all files inside given folder. Returns table of the best matches for each file.
# Eg.: DamerauLevenstheinSlidingForFolderAndTarget("/Documents/Files/DNA", "AAAACCCCTTTTGGGG")
DamerauLevenstheinSlidingForFolderAndTarget <- function(folderPath, targetPattern){
	filesFromFolder	<- GetFilesContentsFromFolder(folderPath)
	results			<-  matrix(0, ncol = 3, nrow = length(filesFromFolder))

	i = 1
	for(fileName in names(filesFromFolder)){
		obsahSouboru <- filesFromFolder[fileName]
		
		result <- DamerauLevenstheinSliding(obsahSouboru, targetPattern)
		results[i, 1] <- fileName
		results[i, 2] <- result[1]
		results[i, 3] <- nchar(obsahSouboru)
		i = i + 1
	}

	colnames(results) <- c("File", "Distance", "Length")
	return( as.data.frame( results ) )
}

SETTINGS_NEEDLEMANWUNSCH_SUBSTMATRIX <- NULL
SETTINGS_NEEDLEMANWUNSCH_GAP_PENALTY <- -100

# Performs Needleman Wunsch scoring for each file inside given folder to the given pattern. Returns score for each file and its length.
NeedlemanWunschOneToAll <- function(folderPath, targetPattern){
	filesFromFolder <- GetFilesContentsFromFolder(folderPath)
	results			<- matrix(,ncol=3, nrow=length(filesFromFolder))
	targetPattern	<- gsub("\\s", "", targetPattern)

	i = 1
	for(fileName in names(filesFromFolder)){
		cat("Working on: ", fileName, "\n")

		content	<- filesFromFolder[fileName]
		content	<- gsub("\\s+", "", content)			# removes new lines, spaces, etc.
		content	<- gsub("\\W+", "", content)			# removes all non-characters

		#score	<- pairwiseAlignment(targetPattern, content, scoreOnly=T, substitutionMatrix=SETTINGS_NEEDLEMANWUNSCH_SUBSTMATRIX)
		x		<- needwunsQS(targetPattern, content, SETTINGS_NEEDLEMANWUNSCH_SUBSTMATRIX, gappen = SETTINGS_NEEDLEMANWUNSCH_GAP_PENALTY)
		score	<- attributes(x)$score

		results[i, 1] = fileName
		results[i, 2] = score
		results[i, 3] = nchar(content)
		i = i+1
	}

	colnames(results)	<- c("File", "Score", "Length")
	return( as.data.frame(results) )
}

MakeNucleotideSubstMatrix <- function(match = 1, mismatch = -1){
	sigma = nucleotideSubstitutionMatrix(match = match, mismatch = mismatch)
	sigma = apply( sigma, 2, as.integer ) 
	rownames(sigma) <- colnames(sigma)
	return(sigma)
}

###################################################################################################
stop_quietly();

## EXAMPLES	#######################################################################################

# 1. Read 3 texts and work with them
X <- GetFileContent("C:/TextSamples/EN2.txt")
Y <- GetFileContent("C:/TextSamples/EN3.txt")
Z <- GetFileContent("C:/TextSamples/EN4.txt")

corpora <- MergePlainTexts( c(X, Y, Z) )

tokens	<- TokenizeText( corpora )
types	<- GetTypes(tokens)

numberOfTokens				<- length(tokens)
numberOfTypes				<- length(types)
meanNumberOfCharsPerToken	<- mean( nchar(tokens) )

hist( nchar(types) )

# 2. Making and using BOW model:

files			<- GetFilesContentsFromFolder("C:/Test")	
tokenizedFiles	<- TokenizeTexts(files)
tokenizedFiles	<- LimitTokensInTexts(tokenizedFiles, count=100, takeRandom=T)
bow				<- MakeBOWModel(tokenizedFiles)			

weights			<- CalculateTFIDFOnBOW(bow)
bowTFIDF		<- ApplyTFIDF(bow, weights)

targetTextBow	<- MakeBOWModel( list(text1=
									TokenizeText(
										GetFileContent("C:/Test/Target/svejk_24.txt"))) )
										
targetTextBowTFIDF	<- ApplyTFIDF(targetTextBow, weights)

# 3. Text searching
DamerauLevenstheinSliding("ABCDEFGH", "FGHJ")

DamerauLevenstheinSlidingForFolderAndTarget("/Documents/Texts/Hasek/Svejk", "oberljonjt")

target <- GetFileContent("/Documents/DNA/Checked/NC_010403.fna")

NeedlemanWunschOneToAll("/Documents/DNA/Files/", target)

# 4. DNA Specific
data(BLOSUM50)
data(BLOSUM45)
data(BLOSUM100)

#protein subst matice:
SETTINGS_NEEDLEMANWUNSCH_SUBSTMATRIX <- BLOSUM50

#nebo nukleotidova:
SETTINGS_NEEDLEMANWUNSCH_SUBSTMATRIX <- MakeNucleotideSubstMatrix(100, -100)  #penalty 100 za shodu a -100 za neshodu
SETTINGS_NEEDLEMANWUNSCH_GAP_PENALTY <- -50

zdroj		= GetFileContent("D:/Mikrosatelity/DNA/Brassica napus clone N2a.txt")
vysledek	= NeedlemanWunschOneToAll("D:/Mikrosatelity/DNA", zdroj)
vysledek	= DamerauLevenstheinSlidingForFolderAndTarget("D:/Mikrosatelity/DNA", zdroj)

#target	<- "AAAACCCCGGGGTTTT"
#content <- "AAAACCCCGGGGTTTT"
#pairwiseAlignment(target, content, scoreOnly=T, substitutionMatrix=SETTINGS_NEEDLEMANWUNSCH_SUBSTMATRIX)
