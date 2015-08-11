# NGGHitApp

This App is developed to explore the hit number and position of any length of input sequence followed with 3 extra flanking nucleotides (_NGG_) searching on the selected genome (human or mouse).

##App execution
To run this App after downloading this App from github to your local PC, please follow the steps as below:

1. If 'Download ZIP', upzip the downloaded file to the Desktop, if 'Clone in Desktop' via git, the whole App folder ("DEAPP-shiny") should be downloaded to the Desktop. 

2. Open R or RStudio (if installed) with R version >= 3.2.

3. Set your working directory to where this App are unzipped or downloaded. 

    For example, I downloaded this App via 'Download ZIP' and unzipped it to the Desktop, then I need to set the working directory in R by `setwd("~/Desktop/NGGHitApp")`.
   
4. Install all depended CRAN R packages and R Bioconductor packages by sourcing the R installation program with `source("install/prep.R")`. 

5. run this App by `shiny::runApp()`

6. A web page will be open in you Browser. Now you can provide your interested sequence, set up genome and mismatch options for your interested sequence+NGG genome search.

##Feedback
If you have further questions or suggestions regarding this App, please contact Yan Li at yli22@bsd.uchicago.edu from the bioinformatics core at the Center for Research Informatics (CRI), biological science division (BSD), University of Chicago.
