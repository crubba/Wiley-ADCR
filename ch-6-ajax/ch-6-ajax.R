### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 6: AJAX
### --------------------------------------------------------------


### 6.1 - 6.2 JavaScript / XHR
### --------------------------------------------------------------

#Loading necessary packages and set working directory
library(XML)
setwd("fortunes/")

#Parsing fortunes1.html - a JavaScript-enriched web sitefortunes1 <- htmlParse("fortunes1.html")

#Parsing an XHR-enriched web sitefortunes2 <- htmlParse("fortunes2.html")

#Requesting the target web site
fortunes_xhr <- getURL("r-datacollection.com/materials/ajax/quotes/quotes.html")