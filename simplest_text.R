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
