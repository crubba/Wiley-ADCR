### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 9: SCRAPING THE WEB
### --------------------------------------------------------------

# load packages
library(RCurl)
library(XML)
library(stringr)
library(plyr)


### 9.3 Web Scraping: Good Practice
### --------------------------------------------------------------

### using a robots.txt parser

# import Facebook's robots.txt
facebook_robotstxt <- "https://www.facebook.com/robots.txt"

# load robots.txt parsing program
source("robots-parser.r")

# check Facebook's robots.txt
robotsCheck(robotstxt = facebook_robotstxt, useragent = "*", dirs = "disallowed")

robotsCheck(robotstxt = facebook_robotstxt, useragent = "Yeti", dirs = "disallowed")


### friendly scraping of minimal data from IMDb page

# parse HTML table
url <- "http://www.imdb.com/chart/top"
top <- getURL(url)
parsed_top <- htmlParse(top, encoding = "UTF-8")
top_table <- readHTMLTable(parsed_top)[[1]]
head(top_table[, 1:3])

# stay identifiable
getURL(url, useragent = str_c(R.version$platform, R.version$version.string, sep = ", "), httpheader = c(from = "eddie@datacollection.com"))

# reduce traffic by monitoring Last-Modified response header and using the If-Modified-Since header
info <- debugGatherer()
httpheader <- list(from = "Eddie@r-datacollection.com", 'user-agent' = str_c(R.version$version.string, ", ", R.version$platform))
handle <- getCurlHandle(debugfunc = info$update, verbose = TRUE)
getBest <- function(doc) readHTMLTable(doc)[[1]][, 1:3]

url <- "http://www.imdb.com/chart/top"
best_doc <- getURL(url)
best_vec <- getBest(best_doc)
if (!file.exists("bestFilms.Rdata")) {
save(best_vec, file = "bestFilms.Rdata")
}
head(best_vec)

httpheader$"If-Modified-Since" <- "Tue, 04 Mar 2014 10:00:00 GMT" # define If-Modified-Since header
best_doc <- getURL(url, curl = handle, httpheader = httpheader)


# using a file's time stamp to determine download activity
writeLines(str_replace_all(getURL("http://www.r-datacollection.com/materials/http/HTTPdate.r"),"\r",""),"httpdate.r") # download httpdate functions
source("httpdate.r")
(last_mod <- file.date("bestFilms.Rdata"))

httpheader$"If-Modified-Since" <- HTTPdate(last_mod)
best_doc <- getURL(url, curl = handle, httpheader = httpheader)

getCurlInfo(handle)$response.code
 
if (getCurlInfo(handle)$response.code == 200) {
best_list <- getBest(best_doc)
save(best_list, file = "bestFilms.Rdata")
}
