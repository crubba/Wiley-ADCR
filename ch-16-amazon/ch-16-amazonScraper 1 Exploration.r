### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 16: Gathering Data on Mobile Phones
### --------------------------------------------------------------

# ------------------------------
### 16.1    Page Exploration ###
# ------------------------------

# ----------------------------------------------------------
### 16.1.1    Searching mobile phones of a specific brand ###
# ----------------------------------------------------------


#### packages we need ####
## ----------------------------------------------------------------------- ##
require(stringr)
require(XML)
require(RCurl)


# creating data folder and changing directory
# ------------------------------------------------------------------------- # 
if(!file.exists("dataExplore")) dir.create("dataExplore")
setwd("dataExplore")


#### doing things step by step ####
## ----------------------------------------------------------------------- ##


## Base URL

# define baseURL and keyword 
baseURL <- "http://www.amazon.com/s/ref=nb_sb_noss_2?url=node%3D2407749011&field-keywords="
keyword <- "Apple"
forceDownload <- F

# convenience function for reading lines and collpase
readLine <- function(x) paste0(readLines(x, warn=F), collapse="\n")

## Get URL

# issue search
fname <- paste0(keyword, " firstSearchPage.html")
if( !file.exists(fname) | forceDownload == T ){
    url             <- paste0(baseURL, keyword)
    firstSearchPage <- getURL(url)
    writeLines(firstSearchPage, fname)
}else{
    firstSearchPage <- readLine(fname)
}

# parse first search page 
parsedFirstSearchPage <- htmlParse(firstSearchPage)


## Restrict search results to certain brand

# extract link to brand restricted search results:
xpath <- paste0('//span[@class="refinementLink" and text()="', keyword,'"]/../@href')
xpath

# execute XPath query and extract URL
restrictedSearchPageLink <- xpathApply(parsedFirstSearchPage, xpath)
restrictedSearchPageLink <- unlist(as.character(restrictedSearchPageLink))
# add base url: http://www.amazon.com
restrictedSearchPageLink <- paste0("http://www.amazon.com",
                                    restrictedSearchPageLink)


## Ask for specific sorting of results
                                    
# add sorting 
restrictedSearchPageLink <- paste0(restrictedSearchPageLink,
                                   "&sort=date-desc-rank")
restrictedSearchPageLink


## Download Search Results
fname <- paste0(keyword, " SearchPage 1.html")
if( !file.exists(fname) | forceDownload == T ){
    restrictedSearchPage <- getURL(restrictedSearchPageLink)
    writeLines(restrictedSearchPage, fname)
}else{
    restrictedSearchPage <- readLine(fname)
}


# extract link for next page
xpath <- "//a[@class='pagnNext']/@href"
parsedRestrictedSearchPage <- htmlParse(restrictedSearchPage)
nextPageLink <- xpathApply(parsedRestrictedSearchPage, xpath)


## Downloading a number of pages 
SearchPages           <- list()
SearchPages[[1]]      <- restrictedSearchPage
xpath           <- "//a[@class='pagnNext']/@href"

# ... as loop until we have 5 SearchPages
for( i in 2:5 ){
    fname <- paste0(keyword, " searchPage ",i,".html")
    if( !file.exists(fname) | forceDownload == T ){
        nextPageLink <- xpathApply( htmlParse(SearchPages[[ i-1 ]]), xpath)
        nextPageLink <- paste0("http://www.amazon.com", nextPageLink)
        SearchPages[[ i ]] <- getURL(nextPageLink)
        writeLines(SearchPages[[ i ]], fname)
    }else{
        SearchPages[[ i ]] <- readLine(fname)
    }
}



# ----------------------------------------------
### 16.1.2    Extracting product information ###
# ----------------------------------------------

SearchPages

# title
xpathApply( htmlParse(SearchPages[[1]]), "//h3/a/span", xmlValue)[1:2]
extractTitle <- function(x){
    unlist(xpathApply( htmlParse(x), "//h3/a/span", xmlValue))
}
titles <- unlist(lapply(SearchPages, extractTitle))

# link
xpathApply( htmlParse(SearchPages[[1]]), "//h3/a", xmlAttrs)[1:2]
extractLink <- function(x){
    unlist(xpathApply( htmlParse(x), "//h3/a", xmlAttrs))
}
links <- unlist(lapply(SearchPages, extractLink))


# get all product pages
chunk <- function(x,n) split(x, ceiling(seq_along(x)/n))
Links <- chunk(links,10)
curl  <- getCurlHandle()
ProductPages  <- list()
counter <- 1 
for(i in 1:length(Links)){
    drange <- counter:(counter+length(Links[[i]])-1)
    fnames <- paste0(keyword, " ProductPage ", drange, ".html")
    if(any(!file.exists(fnames)) | forceDownload==T){
        message("downloading")
        ProductPages <- c(ProductPages, getURL(Links[[i]], curl = curl ))
        wl <- function(text, con) writeLines(unlist(text),con)
        mapply( wl, text=ProductPages[drange], con=fnames) 
        Sys.sleep(0.5)
    }else{
        message("loading from disk")
        ProductPages <- c(ProductPages, lapply(fnames, readLine))
    }
    message(paste(fnames,collapse="\n"))
    counter <- counter+length(Links[[i]])
}
ParsedProductPages <- lapply(ProductPages, htmlParse)


# extract price
unlist( xpathApply( ParsedProductPages[[1]], '//span/@id', as.character ) )
xpathApply( ParsedProductPages[[1]], '//span[@id="actualPriceValue"]', xmlValue )

extractPrice <- function(x){
    x <- xpathApply( x, '//span[@id="actualPriceValue"]', xmlValue )
    x <- unlist(x)
    x <- str_extract(x,"[[:digit:]]*\\.[[:digit:]]*")
    if( length(x)==0 ) x <- NA
    return( as.numeric(x) )
}
prices <- unlist(lapply(ParsedProductPages, extractPrice))
names(prices) <- NULL
prices


# extract stars
unlist(xpathApply( ParsedProductPages[[1]], "//span[contains(@title,' out of 5 stars')]",xmlValue))[1]

extractStar <- function(x){
    x <- xpathApply( x, "//span[contains(@title,' out of 5 stars')]",xmlValue)
    if( length(x)==0 ){
        x <- NA
    }else{
        x <- x[[1]]
        x <- str_extract(x, "[[:digit:]]\\.?[[:digit:]]?")
    }
    return( as.numeric(x) )
}
stars <- unlist(lapply(ParsedProductPages, extractStar))
names(stars) <- NULL
stars

# extract product number (ASIN)
xpathApply( ParsedProductPages[[1]], "//li/b[contains(text(), 'ASIN')]/../text()",xmlValue)

extractASIN <- function(x){
    x <- xpathApply( x, "//li/b[contains(text(), 'ASIN')]/../text()", xmlValue)
    x <- str_trim(unlist(x))
    if( length(x)==0 ) x <- NA
    return( x )
}
asins <- unlist(lapply(ParsedProductPages, extractASIN))
names(asins) <- NULL
asins

# model number
xpathApply( ParsedProductPages[[1]], "//li/b[contains(text(), 'Item model number')]/../text()",xmlValue)

extractModel <- function(x){
    xpath <- "//li/b[contains(text(), 'Item model number')]/../text()"
    x <- xpathApply( x, xpath, xmlValue)
    x <- str_trim(unlist(x))
    if( length(x)==0 ) x <- NA
    return( x )
}
models <- unlist(lapply(ParsedProductPages, extractModel))
names(models) <- NULL
models


# rank 
xpathApply( ParsedProductPages[[1]], "//li[@id='SalesRank']",xmlValue)

extractRank <- function(x){
    x <- xpathApply( x, "//li[@id='SalesRank']", xmlValue)
    x <- unlist(x)
    x <- str_extract(x,"#.*?in")
    x <- str_replace_all(x,"[, in#]","")
    if( length(x)==0 ) x <- NA
    return( as.numeric(x) )
}
ranks <- unlist(lapply(ParsedProductPages, extractRank))
names(ranks) <- NULL
ranks



#### saving to .Rdata file ####
## ----------------------------------------------------------------------- ##

phones <- data.frame(brands="Apple", prices, stars, ranks, asins, models, titles, links, timestamp=as.character(Sys.time()), stringsAsFactors=F)

# save data
save(phones, file="phones.Rdata")



































