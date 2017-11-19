## DEPDENCY #######################################################################################
source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_text.r", encoding="UTF-8")
source("https://raw.githubusercontent.com/oltkkol/vmod/master/basic_ml.r", encoding="UTF-8")

## Example 1 -- Bag Of Words vs language vs Authorship attribution
asimovFiles     <- GetFilesContentsFromFolder("G:/VMOD/DATASETY/AsimovVSFoglar/Asimov/")
foglarFiles     <- GetFilesContentsFromFolder("G:/VMOD/DATASETY/AsimovVSFoglar/Foglar/")

asimovCorpora   <- MergeTexts(asimovFiles)
foglarCorpora   <- MergeTexts(foglarFiles)

asimovTokens    <- TokenizeText(asimovCorpora)
foglarTokens    <- TokenizeText(foglarCorpora)

bowAsimovVsFoglar   <- MakeBOWModel( list(Asimov = asimovTokens, Foglar = foglarTokens) )
weights             <- CalculateTFIDFOnBOW(bowAsimovVsFoglar)

bowTFIDFied         <- ApplyTFIDF(bowAsimovVsFoglar, weights)
