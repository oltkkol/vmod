rlibrary <- function(libraryName, fInstall = NULL){
	prequire <- function() return(require(libraryName, character.only=T))

	if (prequire() == F){
		if (is.function(fInstall)){
			fInstall()
		}else{
			install.packages(libraryName)
		}
		library(libraryName, character.only=T)
	}
}
