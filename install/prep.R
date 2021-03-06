packages <- c("shinydashboard", "DT","shiny", "ggplot2")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

BCpackages <- c("BSgenome.Hsapiens.UCSC.hg19", "BSgenome.Mmusculus.UCSC.mm10")
if (length(setdiff(BCpackages, rownames(installed.packages()))) > 0) {
  source("http://bioconductor.org/biocLite.R")
  biocLite(setdiff(BCpackages, rownames(installed.packages())))
}

sapply(c(packages, BCpackages), require, character.only=T)

print(sapply(c(packages, BCpackages), require, character.only=T))

# sapply(packages, require, character.only=T)
# 
# print(sapply(packages, require, character.only=T))