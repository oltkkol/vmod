library(stringi)
library(stringr)

# reads content of file as plain text
GetFileContent <- function(fileName){
	readedText <- readLines(fileName, encoding="UTF-8")
	readedText <- do.call(paste, c(as.list(readedText), sep=" "))
	return(readedText)
}

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
