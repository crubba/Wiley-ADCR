### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 16: Gathering Data on Mobile Phones
### --------------------------------------------------------------



# --------------------------------
### 16.2    Scraping Procedure ###
# --------------------------------




# ----------------------------------------------------
### 16.2.1    Retrieving data on several producers ###
# ----------------------------------------------------



# loading packages, retrieval and extraction functions
# ------------------------------------------------------------------------- # 
source("amazonScraper 2 Functions.r")


# creating data folder and changing directory
# ------------------------------------------------------------------------- # 
if(!file.exists("dataFull") & grepl("dataFull",basename(getwd()))) dir.create("dataFull")
setwd("dataFull")


# getting data
# ------------------------------------------------------------------------- # 

# option for re-downloading all files or not
forceDownload <- F

# storing keywords
KeyWords <- c("Apple", "BlackBerry", "HTC",
"LG", "Motorola", "Nokia", "Samsung")

# getting search pages
    n <- 5
    SearchPageList <- NULL
    SearchPagesKeywords <- NULL
for(i in seq_along(KeyWords)){
    message(KeyWords[i])
    # get search pages and store them
    SearchPageList <- c(SearchPageList, 
                     getSearchPages(KeyWords[i], n, forceDownload)
                     )
    # store keywords
    SearchPagesKeywords <- c( SearchPagesKeywords, rep(KeyWords[i], n) )
}

# extracting first data
titles  <- extractTitles(SearchPageList)
links   <- extractLinks(SearchPageList)


if(!file.exists("phones.Rdata") | forceDownload==T){
    # getting product pages 
    brands <- rep(KeyWords,each=n*24)
    productPages <- getProductPages(links, brands, forceDownload)

    # extracting further data
    stars  <- extractStars(productPages)
    asins  <- extractASINs(productPages)
    models <- extractModels(productPages)
    ranks  <- extractRanks(productPages)
    prices <- extractPrices(productPages)
    
    fnames <- paste0(brands, " ProductPage ", 1:(length(KeyWords)*n*24),".html" )
    
    # putting data in data frame
    phones <- data.frame(   brands, prices, stars, ranks, asins, models, titles, links, fnames,
                            timestamp=file.info(fnames)$ctime,
                            stringsAsFactors=F)
    
    # save data
    save(phones, stars, asins, models, ranks, prices, brands, timestamp, titles, fnames,
            file="phones.Rdata")
    
}else{
    # load data
    load("phones.Rdata")
}



# ------------------------------
### 16.2.2    Data cleansing ###
# ------------------------------

# drop all NAs
phonesClean <- phones[complete.cases(phones),]

# drop duplicates (ASIN)
phonesClean <- phonesClean[!duplicated(phonesClean$asins),]

# aggregate prices over models
mprices <- rep(NA,length(phonesClean$prices))
for(i in seq_along(phonesClean$models)){
    mprices[i] <- mean(phonesClean$prices[phonesClean$models[i]==phonesClean$models], na.rm=T)
}
phonesClean$prices <- round(mprices,2)

# drop duplicated models
phonesClean <- phonesClean[!duplicated(phonesClean$models),]






# --------------------------------
### 16.3    Graphical Analysis ###
# --------------------------------

# prices, costumer rating and selling rank
# transform ranks to range between 0 and 1 with 1 being good rank
plotResults <- function(X, title="") {
    Prices <- X$prices
    Stars  <- X$stars
    Ranks  <- X$ranks
    plot(  Prices, Stars, pch=20, cex=10, col="white",
           ylim=c(1,5),xlim=c(0,1000), main=title, 
           cex.main=2, cex.axis=2, xaxt="n")
    axis(1, at=c(0,500,1000),labels=c(0,500,1000),cex.axis=2)
    # add guides
    abline(v=seq(0,1000,100),col="grey")
    abline(h=seq(0,5,1),col="grey")
    # adding data
    points(Prices, Stars, col=rgb(0,0,0,0.2), pch=20, cex=7)
    # mark 5 highest values 
    index <- order(Ranks)[1:5]
    abline(v=Prices[index],col="black")
    abline(h=Stars[index],col="black")
    points(Prices[index], Stars[index], col=rgb(1,1,1,1), pch=20, cex=2)
}


pdf("ranks.pdf", width=12, height=6)
  
  # define margins
  par(mfrow=c(2,4))
  par(mar=c(3,2,2,1))
  par(oma=c(3, 4, 0, 1))

  # do plotting
  plotResults(phones, title="all")
  for(i in seq_along(KeyWords)){
    plotResults( phones[phones$brands==KeyWords[i], ], title=KeyWords[i] )
  }
  
  # add common x and y-axis
  mtext("costumer ratings\n",2,outer=T,cex=1.5)
  mtext("prices",1,outer=T,cex=1.5)
dev.off()



