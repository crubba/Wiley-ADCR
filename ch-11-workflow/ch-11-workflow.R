### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 11: Managing Data Projects
### --------------------------------------------------------------

# Loading packages
# ------------------------------------------------------------------------- # 

library(XML)
library(stringr)
library(plyr)
library(RCurl)

### ------------------------------
### 11.2 Processing Multiple Documents/Links ###
### ------------------------------

# 11.2.1 Using for-loops
# ------------------------------------------------------------------------- # 

# get files in directory
all_files <- dir("stocks")
all_files

# using a for-loop
closing_stock <- list()
for(i in 1:length(all_files)){
    path <- str_c("stocks/", all_files[i])
    parsed_stock <- xmlParse(path)
    closing_stock[[i]] <- xpathSApply(parsed_stock, "//Apple", getStock)
}

# create extractor function
getStock <- function(x){
    date <- xmlValue(x[["date"]])
    value <- xmlValue(x[["close"]])
    c(date, value)
}

# turn results into data frame
closing_stock <- unlist(closing_stock)
closing_stock <- data.frame(matrix(closing_stock, ncol = 2, byrow = T))
colnames(closing_stock) <- c("date", "value")

# reclass the contents of the data frame
closing_stock$date <- as.Date(closing_stock$date, "%Y/%m/%d")
closing_stock$value <- as.numeric(as.character(closing_stock$value))

# plot the data
plot(closing_stock$date, closing_stock$value, type = "l", main = "", ylab = "Closing stock", xlab = "Time")

# 11.2.2 Using while-loops and control structures
# ------------------------------------------------------------------------- # 

# run a while loop
a <- 0
while(a < 3){
    a <- a + 1
    print(a)
}

# run a while loop with an if and a break clause

a <- 0
while(TRUE){
    a <- a + 1
    print(a)
    if(a >=3){
        break
    }
}

# while in practice
# Mock URL
url <- "http://www.example.com"

# XPath expression to look for additional pages
xpath_for_next_page <- "//a[@class='NextPage']"

# Create index for pages
i <- 1

# Collect mock URL and write to drive
current_document <- getURL(url)
write(tmp, str_c(i, ".html"))

# Download additional pages while there are linke to additional pages
while(length(xpathSApply(current_document, xpath_for_next_page, xmlGetAttr, "href")) > 0){
    current_url <- xpathSApply(current_document, xpath_for_next_page, xmlGetAttr, "href")
    current_document <- getURL(current_url)
    write(current_document, str_c(i, ".html"))
    i <- i + 1
    
}

# 11.2.3 Using the plyr package
# ------------------------------------------------------------------------- # 

# construct path to XML files
files <- str_c("stocks/", all_files)

# write parser function
getStock2 <- function(file){
    parsedStock <- xmlParse(file)
    closing_stock <- xpathSApply(parsedStock, "//Apple/date | //Apple/close", xmlValue)
    closing_stock <- as.data.frame(matrix(closing_stock, ncol = 2, byrow = T))
}

# run function using ldply

appleStocks <- ldply(files, getStock2)
head(appleStocks, 3)

### ------------------------------
### 11.3   Organizing Scraping Procedures ###
### ------------------------------

# collect links from running exmpale
url <- "http://www.buzzfeed.com"
parsed_page <- htmlParse(url)
links <- xpathSApply(parsed_page, "//a[@href]", xmlGetAttr, "href")
length(links)

# create function
collectHref <- function(url){
    parsed_page <- htmlParse(url)
    links <- xpathSApply(parsed_page, "//[@href]", xmlGetAttr, "href")
    return(links)
}

# run function on various websites
buzzfeed <- collectHref("http://www.buzzfeed.com")
length(buzzfeed)
slate <- collectHref("http://www.slate.com")
length(slate)

# generalizing the function
collectHref <- function(url, begins.http){
    if(!is.logical(begins.http)){
        stop"begins.http must be a logical value")
    }
    parsed_page <- htmlParse(url)
    links <- xpathSApply(parsed_page, "//a[@href]", xmlGetAttr, "href")
    if(begins.http == TRUE){
        links <- links[str_detect(links, "^http")]
    }
    return(links)
}

# run extended function
buzzfeed <- collectHref(url, begins.http = TRUE)
length(buzzfeed)
testPage <- collectHref(url, begins.http = "TRUE")

# set a default for the function
collectHref <- function(url, begins.http = TRUE){
    if(!is.logical(begins.http)){
        stop"begins.http must be a logical value")
    }
    parsed_page <- htmlParse(url)
    links <- xpathSApply(parsed_page, "//a[@href]", xmlGetAttr, "href")
    if(begins.http == TRUE){
        links <- links[str_detect(links, "^http")]
    }
    return(links)
}

# include functionality with source
source("collectHref.r")
test_out <- collectHref("http://www.buzzfeed.com")
length(test_out)

# 11.3.1 Implementation of progress feedback: messages and progress bars
# ------------------------------------------------------------------------- # 

# set up example
baseurl <- "http://www.r-datacollection.com/materials/workflow/stocks"
links <- str_c(baseurl, "/stocks_", 2003:2013, ".xml")

# download files with feedback
N <- length(links)
for(i in 1:N){
    stocks <- getURL(links[i])
    name <- basename(links[i])
    write(stocks, file = str_c("stocks/", name))
    cat(i, "of", N, "\n")
}

# add the name of the file to the feedback
for(i in 1:N){
    stocks <- getURL(links[i])
    name <- basename(links[i])
    write(stocks, file = str_c("stocks/", name))
    cat(i, "of", N, "-", name, "\n")
}

# feedback with if statement
for(i in 1:30){
    if(i %% 10 == 0){
        cat(i, "of", 30, "\n")
    }
}

# write feedback to file
write("", "download.txt")
N <- length(links)
for(i in 1:N){
    stocks <- getURL(links[i])
    name <- basename(links[i])
    write(stocks, file = str_c("stocks/", name))
    feedback <- str_c(i, "of", N, "-", name, "\n", sep = " ")
    cat(feedback)
    write(feedback, "download.txt", append = T)
    write(nchar(stocks), "download.txt", append = T)
    write("------------\n", "download.txt", append = T)
}

# create progress bar
progress_bar <- txtProgressBar(min = 0, max = N, style = 3)
for(i in 1:N){
    stocks <- getURL(links[i])
    name <- basename(links[i])
    write(stocks, file = str_c("stocks/", name))
    setTxtProgressBar(progress_bar, i)
    Sys.sleep(1)
}

# sound feedback
for(i in 1:N){
    tmp <- getURL(website[i])
    write(tmp, str_c(str_replace(websites[i], "http://www.\\.", ""), ".html"))
}
cat("\a")

# 11.3.2 Error and exception handling
# ------------------------------------------------------------------------- # 

# try statements
wrong_pages <- c("http://www.bozzfeed.com", links)
for(i in 1:N){
    url <- try(getURL(wrong_pages[i]))
    if(class(url) != "try_error"){
        name <- basename(wrong_pages[i])
        write(url, name)
    }
}

# set up function and write error log
collectHTML <- function(url){
    html <- getURL(url)
    write(html, basename(url))
}
write("", "error_log.txt")

# tryCatch statements
for(i in 1:N){
    html <- tryCatch(collectHTML(site404[i]), error = function(err){
        errMess <- str_c("Not available - ", site404[i])
        write(str_c(errMess, "error_log.txt"))
    })
}

### ------------------------------
### 11.4   Executing R Scripts on a Regular Basis ###
### ------------------------------

# create downloading routine

if(!file.exists("quotes")) dir.create("quotes")
time <- str_replace_all(as.character(Sys.time()), ":", "_")
fname <- str_c("quotes/rquote ", time, ".html")
url <- "http://www.r-datacollection.com/materials/workflow/rQuotes.php"
download.file(url = url, destfile = fname)
