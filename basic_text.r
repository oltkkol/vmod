## ------------------------------------------------------------------------------------------------
##	DEPENDENCIES + INSTALL
## ------------------------------------------------------------------------------------------------

if (! "Biostrings" %in% installed.packages() ){
	install.packages("stringdist")
	install.packages("stringi")
	install.packages("proxy")
	source("https://bioconductor.org/biocLite.R")
	biocLite("Biostrings")
}

## ------------------------------------------------------------------------------------------------
## 	START HERE
## ------------------------------------------------------------------------------------------------

library(stringi)
library(proxy)
library(stringdist)
library(Biostrings)

Sys.setlocale(category="LC_ALL", locale = "English_United States.1252")
Sys.getlocale(category="LC_ALL")

## BASIC FUNCTIONS FOR TEXT OPERATIONS

# reads content of file as plain text
GetFileContent <- function(fileName){
	 readedText <- readLines(fileName, encoding='UTF-8')
	 readedText <- do.call(paste, c(as.list(readedText), sep=" "))
	 return(readedText)
}

# reads all files and their contents from given folder and saves them into named list
GetFilesContentsFromFolder <- function(folderPath){
	getFileContentFromFolderFile <- function(fileName)	{
		fileName <- paste(folderPath, fileName, sep="/")
		return( GetFileContent(fileName) )
	}

	return( sapply(list.files(folderPath), getFileContentFromFolderFile) )
}

# merges vector of strings to one string
MergeTexts <- function(seznamTextu){
	return( do.call(paste, c(as.list(seznamTextu), sep=" ")) )
}


## TOKENIZACE, TYPY, ATD.	#######################################################################

# basic plain text tokenizer by regular expression split mask
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

# tokenizes vector of texts
TokenizeTexts <- function(texts){
	return( sapply( texts, TokenizeText) )
}
 
# gets types (dictionary) from token list
GetTypes <- function(tokensList){
 return( unique(tokensList) )
}

## BOW MODEL	###################################################################################

# Creates BOW model for named list of texts (eg. readed by GetFilesContentsFromFolder)
MakeBOWModel <- function(texts){
	tokenizedTexts	<- TokenizeTexts(texts)
	allTokens		<- unlist( tokenizedTexts )
	globalDict		<- GetTypes( allTokens )
	globalDict		<- sort( globalDict )

	dataMatrix		<- matrix(ncol = length(globalDict), nrow = length(texts))
	colnames(dataMatrix) <- globalDict
	rownames(dataMatrix) <- names(texts)

	for(textName in names(texts)){
		textTokens <- unlist( tokenizedTexts[textName] )

		for(testedType in globalDict){
			usedTimes <- length( which( textTokens == testedType ) )
			dataMatrix[textName, testedType] <- usedTimes
		}
	}

	return (dataMatrix)
}

## POROVNAVANI TEXTU ##############################################################################

# searches for the most similar word in string by sliding window and calculating DamerauLevensthein distance, returns pair of bestDistance and bestMatch
# eg: DamerauLevenstheinSliding(where = "the tree has dreamed", what = "hat") returns ("1", "has") as the most similar
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

DamerauLevenstheinSlidingForFolderAndTarget <- function(folderPath, targetPattern){	## hleda nejpodobnejsi slovo v plain textech nactenych z adresare, vraci tabulku nejlepsi shody pro kazdy soubor
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
## 	PRIKLADY POUZITI
###################################################################################################

#--------------------------------------------------------------------------------------------------
# Základní práce s textem
#--------------------------------------------------------------------------------------------------

# Naprikald: nacteni tri souboru s EN texty do promennych X, Y, Z a nasledne jejich propojeni do jednoho velkeho souboru
X <- GetFileContent("C:/TextSamples/EN2.txt")
Y <- GetFileContent("C:/TextSamples/EN3.txt")
Z <- GetFileContent("C:/TextSamples/EN4.txt")

vyslednyVelkyText <- MergeTexts( c(X, Y, Z) )

# Ziskani tokenu a typu z textu a par informaci o nich:
tokeny	<- TokenizeText( vyslednyVelkyText )
typy	<- GetTypes(tokeny)

pocetTokenu						<- length(tokeny)
pocetTypu						<- length(typy)
prumernaDelkaTokenuVGrafemech	<- mean( nchar(tokeny) )

hist( nchar(tokeny) )

#--------------------------------------------------------------------------------------------------
# Vytvoreni BoW modelu
#--------------------------------------------------------------------------------------------------

souboryZAdresare	<- GetFilesContentsFromFolder("C:/Test")		## nacte soubory z adresare
data				<- MakeBOWModel(souboryZAdresare)				## vytvori BoW model (matici, radky = jednotlive texty, sloupce = slova, bunky = frekvence zadaneho slova v danem textu)
head(data)												

vysledneSVD			<- svd(data)									## aplikace napr. svd na matici dat


#--------------------------------------------------------------------------------------------------
# Hledani v textech
#--------------------------------------------------------------------------------------------------

DamerauLevenstheinSliding("ABCDEFGH", "FGHJ")

DamerauLevenstheinSlidingForFolderAndTarget("C:/Test", "oberljonjt")

GetFileContent("D:/Temp")

target <- GetFileContent("D:/Temp/NC_010403.fna")
NeedlemanWunschOneToAll("D:/Temp/", target)

#----------------------------------------------------------------------------------------------------
# Porovnavani sekvenci DNA s nastavenim subst matice pro Needlemana
#-----------------------------------------------------------------------------------------------------

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
