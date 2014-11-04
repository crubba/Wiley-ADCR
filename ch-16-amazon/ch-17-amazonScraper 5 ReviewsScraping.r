### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 17: Analyzing Sentiments of Product Reviews
### --------------------------------------------------------------



### -------------------------------
### 17.2    Collecting the data ###
### -------------------------------


### -----------------------------------
### 17.2.1    Downloading the files ###
### -----------------------------------


# loading packages, retrieval and extraction functions
# ------------------------------------------------------------------------- # 
require(stringr)
require(XML)
require(RCurl)
require(RSQLite)



# connecting to DB
# ------------------------------------------------------------------------- # 
# connection
sqlite  <- dbDriver("SQLite")
con     <- dbConnect(sqlite, "amazonProductInfo.db")

# getting phones data 
sql <- "SELECT phones.asin, items.fname FROM phones 
        JOIN items
        ON phones.id=phones_id;"
phonesData <- dbGetQuery(con, sql)



# creating data folder and changing directory
# ------------------------------------------------------------------------- # 
if(!file.exists("dataReviews")) dir.create("dataReviews")
setwd("dataReviews")



# constructing links to reviews
# ------------------------------------------------------------------------- # 

# parse product pages
productPageFiles <- paste0("../dataFull/", phonesData$fname)
productPages     <- lapply(productPageFiles, htmlParse)

# extract links to review pages 
extractReviewLinks <- function(x){
    x <- xpathApply(x, "//a[contains(text(), 'customer review')]/@href", as.character)[[1]]
    if(length(x)==0) x <- NA
    if(str_detect(x,"create-review") & !is.na(x)) x <- NA
    names(x) <- NULL
    x
}
reviewLinks <- unlist(lapply(productPages, extractReviewLinks))

# check why there is no review page link
for(i in seq_along(reviewLinks)){
    if(is.na(reviewLinks[i])){
        link <- dirname(getwd())
        link <- paste0(link,str_replace(productPageFiles[i],"\\.\\.",""))
        print(link)
        #browseURL(link)
    }
}

# add host
reviewLinks <- str_replace(reviewLinks,"http://www.amazon.com","")
reviewLinks <- ifelse(is.na(reviewLinks),NA,paste0("http://www.amazon.com",reviewLinks))



# downloading reviews
# ------------------------------------------------------------------------- # 

# loop through reviewLinks and download pages
for(i in seq_along(reviewLinks)){
    # file name
    fname <- paste0(phonesData[i,"asin"], "_0001.html")
    # download
    if(!file.exists(fname) & !is.na(reviewLinks[i])){
        message("downloading")
        try(download.file(reviewLinks[i],fname))
        # sleep
        sleep <- abs(rnorm(1)) + runif(1,0,0.25)
        message(paste0("I have done ",i," of ",length(reviewLinks)," - gonna sleep ",round(sleep,2)," seconds."))
        Sys.sleep(sleep)
    # size of file info
    }
    message(paste0(i," size: ",file.info(fname)$size/1000)," KB")
}


# list of file names for first review pages // delete files that are of zero size
firstPages <- list.files(pattern="001.html")
file.remove(firstPages[file.info(firstPages)$size==0])
firstPages <- list.files(pattern="001.html")


# parse files
HTML <- lapply(firstPages, htmlParse)



# downloading next pages
# ------------------------------------------------------------------------- # 

# loop through HTML and get all next pages
for(i in seq_along(HTML)){
    # extract link
    link <- xpathApply( HTML[[i]],
                        "//a[contains(text(), 'Next')]/@href", 
                        as.character)[[1]]
    # set k to 2
    k <- 2
    while(length(link) > 0 & k <= 5){
        # gen filename
        fname <- str_replace(   firstPages[i], "[[:digit:]]{4}.html",
                                paste0(str_pad(k, 4, side = "left", pad = "0"),".html"))
        message(paste0(i,":",k,"... :",fname))
        # download file
        if(!file.exists(fname) & length(link) > 0){
            download.file(link, fname, quiet=T)
            message(paste0(" download to file name: ",fname))
            Sys.sleep(abs(rnorm(1)) + runif(1,0,0.25))
        }
        htmlNext <- htmlParse(fname)
        # extract link for next file
        link <- tryCatch(xpathApply( htmlNext,
                        "//a[contains(text(), 'Next')]/@href", 
                        as.character)[[1]], 
                        error = function(e){
                        message("xpath error")
                        NULL
                        })
        # k +1
        k <- k + 1
    }
}



# delete files of zero size or near zero size (download has not worked here)
tmp <- list.files(pattern=".html")
file.remove(tmp[file.info(tmp)$size < 50000])





### ------------------------------------
### 17.2.2    Information extraction ###
### ------------------------------------

# extracting meta infos
# ------------------------------------------------------------------------- # 

# convenience function for extracting only numbers from node
getNumbers <- function(node){
    val <- xmlValue(node)
    x <- str_extract(val,"[[:digit:]]{1,6}")
    x
}

FPAsins <- str_replace(firstPages,"_0001.html","")
reviewsMeta   <- data.frame(asin=FPAsins,one=NA,two=NA,three=NA,four=NA,five=NA,stringsAsFactors=F)

# extraction of review meta data
for(i in seq_along(HTML)){
    tmp <- as.numeric( 
            readHTMLTable(  HTML[[i]],
                            elFun = getNumbers,
                            stringsAsFactors=F
                         )$productSummary$V3 )
    print(tmp)
    reviewsMeta[i,c("one","two","three","four","five")] <- tmp[1:5]
}

reviewsMeta$sum <- apply(reviewsMeta[,c("one","two","three","four","five")],1,sum)
reviewsMeta$mean <- ( reviewsMeta$one + reviewsMeta$two*2 + reviewsMeta$three*3 + 
                      reviewsMeta$four*4 + reviewsMeta$five*5) /    reviewsMeta$sum
              

              
# extracting review specific infos
# ------------------------------------------------------------------------- # 
reviews <- data.frame(  asin=NA, stars=0, helpfulyes=0, helpfulno=0, 
                        helpfulsum=0, date="", title="",text="", stringsAsFactors=F)
# loop through asins
for(i in seq_along(FPAsins)){
    files <- list.files(pattern=FPAsins[i])
    asin  <- FPAsins[i]
    
    # loop through files with same asin
    message(paste0(i," / ",length(FPAsins)," ... doing: ",asin))
    for(k in seq_along(files)){
        html <- htmlParse(files[k])
        
        # base path for each review : "//div[@style='margin-left:0.5em;']"
        reviewValue  <- unlist(xpathApply(html, "//div[@style='margin-left:0.5em;']",xmlValue))
        
        # helpful
        helpful      <- str_extract(reviewValue,"[[:digit:]]{1,5}.*?[[:digit:]]{1,5} people")
        helpful      <- str_extract_all(helpful,"[[:digit:]]{1,5}")
        helpfulyes   <- as.numeric(unlist(lapply(helpful,'[',1)))
        helpfulno    <- as.numeric(unlist(lapply(helpful,'[',2))) - helpfulyes
        helpfulsum   <- helpfulyes + helpfulno
        
        # stars
        stars        <- str_extract(reviewValue,"[[:digit:]]\\.[[:digit:]] out of 5 stars")
        stars        <- as.numeric(str_extract(stars, "[[:digit:]]"))
        
        # text
        text <- unlist(xpathApply( html, 
                                   "//div[@style='margin-left:0.5em;']/div[@class='reviewText']",
                                   xmlValue))
        text <- str_replace_all(text, "'","''")
        
        # title
        title <- xpathApply(
          html, 
          "//div[@style='margin-left:0.5em;']/div/span[@style='vertical-align:middle;']/b", 
          xmlValue)
        title  <- unlist(title)
        title <- str_replace_all(title, "'","''")
        
        # date
        date <- xpathApply(
          html,
          "//div[@style='margin-left:0.5em;']/div/span[@style='vertical-align:middle;']/nobr", 
          xmlValue)
        }
        date <- unlist(date)
        
        # putting it together
        tmp <- cbind(asin,stars,helpfulyes,helpfulno,helpfulsum,date,title,text)
        reviews <- rbind(reviews,tmp)
}

# delete where asin is NA
reviews <- reviews[!is.na(reviews$asin),]




### ------------------------------
### 17.2.3    Database storage ###
### ------------------------------


# writing data to DB
# ------------------------------------------------------------------------- # 

# add meta data 
SQL <- paste0(" INSERT INTO reviewsMeta 
                (asin, one, two, three, four, five) 
                VALUES 
                ('",
                paste(  reviewsMeta[,"asin"], reviewsMeta[,"one"],
                        reviewsMeta[,"two"],  reviewsMeta[,"three"],
                        reviewsMeta[,"four"], reviewsMeta[,"five"],
                        sep="', '")
                ,"'); ")
for(i in seq_along(SQL)) dbGetQuery(con,SQL[i])

# add review data
SQL <- paste0(" INSERT INTO reviews 
                ( asin, stars, helpfulyes, helpfulno, 
                  helpfulsum, date, title, text) 
                VALUES 
                ('",
                paste(  reviews[,"asin"], reviews[,"stars"],
                        reviews[,"helpfulyes"],  reviews[,"helpfulno"],
                        reviews[,"helpfulsum"], reviews[,"date"],
                        reviews[,"title"], reviews[,"text"],
                        sep="', '")
                ,"'); ")
for(i in seq_along(SQL)){
 dbGetQuery(con,SQL[i])
 }












