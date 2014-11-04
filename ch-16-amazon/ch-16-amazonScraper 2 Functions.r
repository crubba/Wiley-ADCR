### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 16: Gathering Data on Mobile Phones
### --------------------------------------------------------------

# --------------------------------
### 16.2    Scraping Procedure ###
# --------------------------------

# packages
require(stringr)
require(XML)
require(RCurl)
require(RSQLite)

# convenience function for reading lines and collapse
readLine <- function(x) paste0(readLines(x), collapse="\n")

# a general option for useragent:
useRagent <- "eddie@r-datacollection.com"


# function for downloading first search page
getFirstPage <- function(keyword, forceDownload=F ){
    # base URL
    baseURL <- "http://www.amazon.com/s/ref=nb_sb_noss_2?url=node%3D2407749011&field-keywords="

    # issue search
    fname <- paste0(keyword, " SearchPage 0.html")
    if( !file.exists(fname) | forceDownload == T ){
        message("downloading first search page")
        url             <- paste0(baseURL, keyword)
        firstSearchPage <- getURL(url, useragent=useRagent)
        writeLines(firstSearchPage, fname)
    }else{
        message("loading first search page from disk")
        firstSearchPage <- readLine(fname)
    }
    
    # extract link to brand restricted search results:
    xpath <- paste0('//span[@class="refinementLink" and text()="', keyword,'"]/../@href')
    
    # execute XPath query and extract URL
    restrictedSearchPageLink <- xpathApply(htmlParse(firstSearchPage), xpath)
    restrictedSearchPageLink <- unlist(as.character(restrictedSearchPageLink))
    
    # add base url: http://www.amazon.com
    restrictedSearchPageLink <- paste0("http://www.amazon.com",
                                    restrictedSearchPageLink)
    
    # add sorting 
    restrictedSearchPageLink <- paste0(restrictedSearchPageLink,
                                   "&sort=salesrank")
    restrictedSearchPageLink
    
    # download brand restricted and sorted first search page
    fname <- paste0(keyword, " SearchPage 1.html")
    if( !file.exists(fname) | forceDownload == T ){
        message("downloading restricted search page")
        restrictedSearchPage <- getURL(restrictedSearchPageLink, useragent=useRagent)
        writeLines(restrictedSearchPage, fname)
    }else{
        message("loading restricted search page from disk")
        restrictedSearchPage <- readLine(fname)
    }
    
    # return brand restricted and sorted first search page
    return(restrictedSearchPage)
}
# firstPage <- getFirstPage("Apple", F)


# function for downloading next page
getNextSearchPage <- function(PreviousSearchPage, fname="", forceDownload=F){
    if( !file.exists(fname) | forceDownload==T ){
        message("downloading next page")
        # specify xpath
        xpath <- "//a[@class='pagnNext']/@href"
        # extract link
        nextPageLink <- xpathApply( htmlParse(PreviousSearchPage), xpath)
        nextPageLink <- paste0("http://www.amazon.com", nextPageLink)
        # download page
        nextPage <- getURL(nextPageLink, useragent=useRagent)
        # save to disk
        writeLines(nextPage, fname)
    }else{
        message("loading next page from disk")
        # load next page from disk
        nextPage <- readLine(fname)
    }
    # return
    return(nextPage)
}
# nextPage <- getNextSearchPage(firstPage, fname=paste0(keyword," SearchPage ", 2,".html") )


# wrapper function returning a list of search pages
getSearchPages <- function(keyword, n, forceDownload=F){
    # create list for storage
    SearchPages           <- list()
    # get first search page
    SearchPages[[1]]      <- getFirstPage(keyword, forceDownload)
    # ... as loop until we n is reached
    while( length(SearchPages) < n ){
        i <- length(SearchPages) + 1
        fname <- paste0(keyword, " SearchPage ",i,".html")
        SearchPages[[ i ]] <- getNextSearchPage(SearchPages[[i-1]], fname, forceDownload)
    }
    # return list
    return(SearchPages)
}
# AppleSearchPages <- getSearchPages("Apple", 10, F)


# function for extracting titles
extractTitles <- function(SearchPages){
    # function for one extraction
    extractTitle <- function(x){
        unlist(xpathApply( htmlParse(x), "//h3/a/span", xmlValue)[1:24])
    }
    # applying function to a set
    titles <- unlist(lapply(SearchPages, extractTitle))
    # return titles
    names(titles) <- NULL
    return(titles)
}
# AppleTitles <- extractTitles(AppleSearchPages)


# function for extracting links to product pages from search pages
extractLinks <- function(SearchPages){
    extractLink <- function(x){
        unlist(xpathApply( htmlParse(x), "//h3/a", xmlAttrs)[1:24])
    }
    links <- unlist(lapply(SearchPages, extractLink))
    # return
    names(links) <- NULL
    return(links)
}
# AppleLinks <- extractLinks(AppleSearchPages)


# function for downloading product pages
getProductPages <- function(links, keyword, forceDownload=F){
    # splitting vector in chunks
    chunk <- function(x,n) split(x, ceiling(seq_along(x)/n))
    Links <- chunk(links,6)
    # preparation: create curl handle, list for storage, set counter to 1
    curl = getCurlHandle()
    ProductPages  <- list()
    counter <- 1 
    # cycle through chunks of links
    for(i in 1:length(Links)){
        # range of links which are in the present chunk 
        drange <- counter:(counter+length(Links[[i]])-1)
        kwords <- keyword[drange]
        # file names for storage on disk
        fnames <- paste0(kwords, " ProductPage ", drange, ".html")
        if(any(!file.exists(fnames)) | forceDownload==T){
            # downloading
            message("\ndownloading ...")
            ProductPages <- c( ProductPages, 
                                getURL( Links[[i]], curl = curl, useragent=useRagent) 
                              )
            # save to file
            wl <- function(text,con) writeLines(unlist(text),con)
            mapply( wl, text=ProductPages[drange], con=fnames) 
            Sys.sleep(0.5)
        }else{
            # loading from disk
            message("loading from disk ...")
            ProductPages <- c(ProductPages, lapply(fnames, readLine))
        }
        # parsing
        message(paste(fnames,collapse="\n"))
        counter <- counter+length(Links[[i]])
    }
    message("parsing product pages")
    ParsedProductPages <- lapply(ProductPages, htmlParse)
    # return product pages
    return(ParsedProductPages)
}
# AppleProductPages <- getProductPages(AppleLinks, rep("Apple",length(AppleLinks)))



# function for extracting stars
extractStars <- function(ParsedProductPages){
    extractStar <- function(x){
        # extraction of info
            tmp <- xpathApply( x, 
                "//span[@class='reviewCountTextLinkedHistogram']/@title",
                as.character)
            tmp <- unlist(tmp)[1]
        if(length(tmp)==0){
            x <- xpathApply( x, 
                "//span[contains(@class,'swSprite')]/@title", 
                as.character)
            x <- unlist(x)[1]
        }else{
            x <- tmp
        }
        # cleansing
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
    # return stars
    return(stars)
}
# extractStars(productPages)


# function for extracting product number (ASIN)
extractASINs <- function(ParsedProductPages){
    extractASIN <- function(x){
        x <- xpathApply( x, "//input[@id='ASIN']/@value", as.character)
        x <- unlist(x)
        if( length(x)==0 ) x <- NA
        return( x )
    }
    asins <- unlist(lapply(ParsedProductPages, extractASIN))
    # return ASINs
    return(asins)
}
# extractASINs(AppleProductPages)


# function for extracting model numbers
extractModels <- function(ParsedProductPages){
    extractModel <- function(x){
        xpath <- "//li/b[contains(text(), 'Item model number')]/../text()"
        x <- xpathApply( x, xpath, xmlValue)
        x <- unlist(x)
        if( length(x)==0 ) x <- NA
        return( x )
    }
    models <- unlist(lapply(ParsedProductPages, extractModel))
    # return models
    return(models)
}
# extractModels(AppleProductPages)


# function for extracting Amazon selling rank 
extractRanks <- function(ParsedProductPages){
    extractRank <- function(x){
        x <- xpathApply( x, "//li[@id='SalesRank']", xmlValue)
        x <- unlist(x)
        x <- str_extract(x,"#.*?in")
        x <- str_replace_all(x,"[, in#]","")
        if( length(x)==0 ) x <- NA
        return( as.numeric(x) )
    }
    ranks <- unlist(lapply(ParsedProductPages, extractRank))
    # return ranks
    return(ranks)
}
# extractRanks(AppleProductPages)


# function for extracting prices
extractPrices <- function(ParsedProductPages){
    extractPrice <- function(x){
            tmp <- xpathApply( x, '//span[@id="priceblock_ourprice"]', xmlValue )
        if(length(tmp)==0){
            x   <- xpathApply( x, '//span[@id="actualPriceValue"]', xmlValue )
        }else{
            x <- tmp
        }
        x <- unlist(x)
        x <- str_extract(x,"[[:digit:]]*\\.[[:digit:]]*")
        if( length(x)==0 ) x <- NA
        return( as.numeric(x) )
    }
    prices <- unlist(lapply(ParsedProductPages, extractPrice))
    # return prices
    return(prices)
}


    
    
    
    
    
    
    
    
    
    
    
    