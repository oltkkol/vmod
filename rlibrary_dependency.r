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

stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

rlibrary("brew")
rlibrary("caTools")
rlibrary("e1071")
rlibrary("MASS")
rlibrary("datasets")
rlibrary("e1071")
rlibrary("mlbench")
rlibrary("stringr")
rlibrary("klaR")
rlibrary("ff")
rlibrary("hash")

rlibrary("stringi")
rlibrary("proxy")
rlibrary("stringdist")
rlibrary("stringr")

rlibrary("ggplot2")
rlibrary("grid")
rlibrary("gridExtra")
rlibrary("xtable")
rlibrary("ggExtra")

rlibrary("Metrics")
rlibrary("minpack.lm")
rlibrary("MLmetrics")
rlibrary("snow")
rlibrary("doSNOW")
rlibrary("parallel")

rlibrary("Biostrings", function(){
	source("https://bioconductor.org/biocLite.R")
	biocLite("Biostrings")
})

rlibrary("mxnet", function(){
	cran <- getOption("repos")
	cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
	options(repos = cran)
	install.packages("mxnet")
	# see http://mxnet.incubator.apache.org/get_started/install.html
})

Sys.setlocale(category="LC_ALL", locale = "English_United States.1252")
Sys.getlocale(category="LC_ALL")

cat("Dependency lib loaded.")
