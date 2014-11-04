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


### 9.1 Retrieval Scenarios
### --------------------------------------------------------------

### 9.1.1 Downloading ready-made files ---------------------------

### Scenario: download list of CSV files
# Download state-, county-, and precinct-level election results for the 2012 Presidential election in Maryland
# Data available in csv format at http://www.elections.state.md.us/elections/2012/election_data/index.html
# More information on the files at http://www.elections.state.md.us/elections/using_election_data_instructions.html

# set local directory
setwd("scenario-maryland")

# prepare filenames
u <- "http://www.elections.state.md.us/elections/2012/election_data/index.html"
page_parse <- htmlParse(u, encoding = "utf-8")
links <- getHTMLLinks(u)
filenames <- links[str_detect(links, "_General.csv")]
filenames_list <- as.list(filenames)
filenames_list

# write download function
downloadCSV <- function(filename, baseurl, folder) {
	dir.create(folder, showWarnings = FALSE)
	fileurl <- str_c(baseurl, filename)
	if (!file.exists(str_c(folder, "/", filename))) {
		download.file(fileurl,
		 destfile = str_c(folder, "/", filename))
		Sys.sleep(1)
	}
}

# execute download
l_ply(filenames_list, downloadCSV,
 baseurl = "http://www.elections.state.md.us/elections/2012/election_data/",
 folder = "elec12_maryland")

# check results
length(list.files("./elec12_maryland"))
list.files("./elec12_maryland")[1:5]


### Scenario: download list of binary files (PDF files)
# now: download PDF files on Maryland legislative district maps from http://planning.maryland.gov/Redistricting/2010/legiDist.shtml

# prepare filenames
url <- "http://planning.maryland.gov/Redistricting/2010/legiDist.shtml"
links <- getHTMLLinks(url)
filenames <- links[str_detect(links, "2010maps/Leg/Districts_")]
filenames_list <- str_extract_all(filenames, "Districts.+pdf")
basename(filenames)

# write download function
downloadPDF <- function(filename, baseurl, folder, handle) {
	dir.create(folder, showWarnings = FALSE)
	fileurl <- str_c(baseurl, filename)
	if (!file.exists(str_c(folder, "/", filename))) {
		pdf_file <- getBinaryURL(fileurl, curl = handle)
		writeBin(pdf_file, str_c(folder, "/", filename))
		Sys.sleep(1)
	}
}

# execute download
handle <- getCurlHandle()
handle <- getCurlHandle(useragent = str_c(R.version$platform, R.version$version.string, sep=", "), httpheader = c(from = "eddie@datacollection.com"))
l_ply(filenames_list, downloadPDF,
 baseurl = "planning.maryland.gov/PDF/Redistricting/2010maps/Leg/",
 folder = "elec12_maryland_maps",
 handle = handle)

# check results
length(list.files("./elec12_maryland_maps"))
list.files("./elec12_maryland_maps")[1:5]



### 9.1.2 Downloading multiple files from an FTP index -----------

# download CRAN task view html files from ftp://cran.r-project.org/pub/R/web/views/

# generally helpful parameters when dealing with FTP servers:  ftpport, dirlistonly, quote, postquote, prequote, ftp.use.eprt, ftp.use.epsv, ftp.create.missing.dirs, ftp.response.timeout, ftp.ssl, ftpappend, ftplistonly, transfertext.

# set local directory
setwd("scenario-ftp")

# define requested resource
ftp <- 'ftp://cran.r-project.org/pub/R/web/views/'

# we have to get the filenames using another apprach, because...
getHTMLLinks(ftp) # does not work

# prepare filenames
ftp_files <- getURL(ftp, dirlistonly = TRUE)
cat(ftp_files)

filenames <- strsplit(ftp_files, "\r\n")[[1]]
filenames_html <- unlist(str_extract_all(filenames, ".+(.html)"))
filenames_html

# prepare filenames, alternative with FTP command 'NLST' which 'returns a list of file names in a specified directory'
filenames_html <- getURL(ftp, customrequest = "NLST *.html")
filenames_html = strsplit(filenames_html, "\\\r\\\n")[[1]]
filenames_list <- as.list(filenames_html)

# write download function
downloadFTP <- function(filename, folder, handle) {
	dir.create(folder, showWarnings = FALSE)
	fileurl <- str_c(ftp, filename)
	if (!file.exists(str_c(folder, "/", filename))) {
		datafile <- try(getURL(fileurl, curl = handle))
		write(datafile, str_c(folder, "/", filename))
		Sys.sleep(1)
	}
}

# execute download
handle <- getCurlHandle(ftp.use.epsv = FALSE)
l_ply(filenames_html, downloadFTP, folder = "cran_tasks", handle = handle)

# check results
length(list.files("./cran_tasks"))
list.files("./cran_tasks")[1:5]


### 9.1.3 Manipulating URLs to access multiple pages ------------

# download press releases from http://www.transparency.org/news/pressreleases/year/2010 from the year 2010 which are scattered over several pages

# strategy:
	# change in the URL when switching to a new site changes systematically
	# replicate this change and manually construct the set of URLs with the desired resources

# set local directory
setwd("scenario-urlmanipulation")

# getPageURLs function
getPageURLs <- function(url) {
	baseurl <- htmlParse(url)
	total_pages <- as.numeric(xpathSApply(baseurl, "//div[@id='Page']/strong[2]", xmlValue))
	max_url <- (total_pages - 1)*10
	add_url <- str_c("/P", seq(10, max_url, 10))
	urls_list <- as.list(str_c(url, add_url))
	urls_list[length(urls_list) + 1] <- url
	return(urls_list)
}

url <- "http://www.transparency.org/news/pressreleases/year/2010"
urls_list <- getPageURLs(url)

# dlPages function
dlPages <- function(pageurl, folder ,handle) {
	dir.create(folder, showWarnings = FALSE)
	page_name <- str_c(str_extract(pageurl, "/P.+"), ".html")
	if (page_name == "NA.html") { page_name <- "/base.html" }
	if (!file.exists(str_c(folder, "/", page_name))) {
		content <- try(getURL(pageurl, curl = handle))
		write(content, str_c(folder, "/", page_name))
		Sys.sleep(1)
	}
}

handle <- getCurlHandle()
l_ply(urls_list, dlPages, folder = "tp_index_2010", handle = handle)
list.files("tp_index_2010")[1:3]

# getPressURLs function
getPressURLs <- function(folder) {
pages_parsed <- lapply(str_c(folder, "/", dir(folder)), htmlParse)
urls <- unlist(llply(pages_parsed, getHTMLLinks))
# urls <- unlist(llply(pages_parsed, xpathSApply, "//a/@href"))
press_urls <- urls[str_detect(urls, "http.+/pressrelease/")]
press_urls_list <- as.list(press_urls)
return(press_urls_list)
}
press_urls_list <- getPressURLs(folder = "tp_index_2010")

# dlPress function
dlPress <- function(press_url, folder, handle) {
	dir.create(folder, showWarnings = FALSE)
	press_filename <- str_c(str_extract(press_url, "[^//][[:alnum:]_.]+$") , ".html")
	if (!file.exists(str_c(folder, "/", press_filename))) {
		content <- try(getURL(press_url, curl = handle))
		write(content, str_c(folder, "/", press_filename))
		Sys.sleep(1)
	}
}
handle <- getCurlHandle()
l_ply(press_urls_list, dlPress, folder = "tp_press_2010", handle = handle)
					 
					 

### 9.1.4 Convenient functions to gather links, lists and tables from HTML documents ------------

# set local directory
setwd("scenario-xmlconvenience")

# parse document
mac_url    <- "http://en.wikipedia.org/wiki/Machiavelli"
mac_source <- readLines(mac_url, encoding = "UTF-8")
mac_parsed <- htmlParse(mac_source, encoding = "UTF-8")
mac_node   <- mac_parsed["//p"][[1]]

# getHTMLLinks()
getHTMLLinks(mac_url)[1:3]
getHTMLLinks(mac_source)[1:3]
getHTMLLinks(mac_parsed)[1:3]
getHTMLLinks(mac_node)[1:3]

# getHTMLLinks() with XPath expression
getHTMLLinks(mac_source, xpQuery = "//a[@class='extiw']/@href")[1:3]

# getHTMLExternalFiles()
xpath <- "//img[contains(@src, 'Machiavelli')]/@src"
getHTMLExternalFiles(mac_source, xpQuery = xpath)[1:3]

# readHTMLList()
readHTMLList(mac_source)
readHTMLList(mac_source)[[10]][1:3]

# readHTMLTable()
names(readHTMLTable(mac_source))
readHTMLTable(mac_source)$persondata
readHTMLTable(mac_source, stringsAsFactors = F)[[1]][7:8,1]

# modify readHTMLTable()
influential <- readHTMLTable(mac_source,
elFun = getHTMLLinks,              stringsAsFactors = FALSE)[[1]][7,]
as.character(influential)[1:3]

influenced <- readHTMLTable(mac_source,
elFun = getHTMLLinks, stringsAsFactors = FALSE)[[1]][8,]
as.character(influenced)[1:3]


### 9.1.5 Dealing with HTML forms --------------------------------

# Filling out forms in the browser:
	# fill out the form,
	# push the submit, ok, start or the like! button. 
	# let the browser execute the action specified in the source code of the form and send the data to the server,
	# and let the browser receive the returned resources after the server has evaluated the inputs. 

# Using forms in scraping practice:
	# recognize that forms are involved,
	# determine the method used to transfer the data,
	# determine the address to send the data to,
	# determine the inputs to be sent along,
	# build a valid request and send it out, and
	# process the returned resources. 

# set local directory
setwd("scenario-forms")


### GET forms
# goal: gathering data from Princeton WordNet at http://wordnetweb.princeton.edu/perl/webwn

# query function
query <- "trust"
wordnetFun <- function(query) {
url <- sprintf("http://wordnetweb.princeton.edu/perl/webwn?s=%s&sub=Search+WordNet", query)
getURL(url)
}

# setup
info   <- debugGatherer()
handle <- getCurlHandle(cookiejar      = "", 
                           followlocation = TRUE, 
                           autoreferer    = TRUE,
                           debugfunc      = info$update,
                           verbose        = TRUE,
                           httpheader     = list(
                             from         = "Eddie@r-datacollection.com",
                             'user-agent' = str_c(R.version$version.string, 
                                              ", ", R.version$platform)
                           ))

# convenience function to make data frame from attributes
xmlAttrsToDF <- function(parsedHTML, xpath){
	x <- xpathApply(parsedHTML, xpath, xmlAttrs)
	x <- lapply(x, function(x) as.data.frame(t(x)))
	do.call(rbind.fill, x)
}

# retrieve form
url        <- "http://wordnetweb.princeton.edu/perl/webwn"
html_form   <- getURL(url, curl = handle)
parsed_form <- htmlParse(html_form)

# inspect form
xmlAttrsToDF(parsed_form, "//form") # again, compare the result of this function with:
xpathSApply(parsed_form, "//form")

# inspect parameters
xmlAttrsToDF(parsed_form, "//form[1]/input")

# send GET request
html_form_res   <- getForm(uri=url, curl=handle, s="data")
parsed_form_res <- htmlParse(html_form_res)
xpathApply(parsed_form_res,"//li", xmlValue)

# inspect request header information
cat(str_split(info$value()["headerOut"], "\r")[[1]])
info$reset()


### POST forms
# gathering data from read-able at http://read-able.com/

# post form
url  <- "http://read-able.com/"
form <- htmlParse(getURL(url = url, curl = handle))
xmlAttrsToDF(form, "//form")  

xmlAttrsToDF(form, "//form[2]//input")
xpathApply(form, "//form[2]")

sentence <- '"It is a capital mistake to theorize before one has data. Insensibly one begins to twist facts to suit theories, instead of theories to suit facts." - Arthur Conan Doyle, Sherlock Holmes'

res <- postForm(uri=str_c(url, "check.php"), 
                curl=handle,
                style="POST",
                directInput=sentence)

readHTMLTable(res)

# inspect request headers
cat(str_split(info$value()["headerOut"],"\r")[[1]])
cat(str_split(info$value()["dataOut"],"\r")[[1]])
info$reset()


# transforming images at http://www.fixpicture.org/

# get image
url <- "r-datacollection.com/materials/http/sky.png"
sky <- getBinaryURL(url=url)
writeBin(sky, "sky.png")

handle <- getCurlHandle()
# get url
url   <- "http://www.fixpicture.org/"
form  <- htmlParse(getURL(url = url, curl = handle))

# get form specs
xmlAttrsToDF(form, "//form") 
xmlAttrsToDF(form, "//input")[1:2 ,c("name","type","class","value")] 
xmlAttrsToDF(form, "//select")
xmlAttrsToDF(form, "//select/option")

# send picture
res <- postForm(uri = "http://www.fixpicture.org/resize.php?LANG=en", 
                 image = fileUpload(filename = "sky.png", 
                                    contentType = "image/png"),
                 format = "pdf",
                 curl = handle)

# retrieve result link
doc  <- htmlParse(res)
link <- str_c(url, xpathApply(doc,"//a/@href", as.character)[[1]])

# store image
resImage <- getBinaryURL(link, curl=handle)
writeBin(resImage,"sky.pdf",useBytes=T)


### Automating form handling with the RHTMLForms package

# basic procedure with RHTMLForms
	# use getHTMLFormDescription() on the URL where the HTML form is located and save its results in an object -- let's call it forms
	# use createFunction() on the first item of the forms object and save the results in another object, say form_function
	# formFunction() takes input fields as options to send them to the server and return the result. 


install.packages("RHTMLForms", 
                 repos ="http://www.omegahat.org/R", 
                 type  ="source")
library(RHTMLForms)

url          <- "http://wordnetweb.princeton.edu/perl/webwn"
forms        <- getHTMLFormDescription(url)
formFunction <- createFunction(forms[[1]])

html_form_res   <- formFunction(s = "data", .curl = handle)
parsed_form_res <- htmlParse(html_form_res)
xpathApply(parsed_form_res,"//li", xmlValue)

cat(str_split(info$value()["headerOut"],"\r")[[1]])



### 9.1.6 HTTP authentication ------------------------------

url <- "www.r-datacollection.com/materials/solutions"
cat(getURL(url, userpwd = "teacher:sesame", followlocation = TRUE))

options(RDataCollectionLogin = "teacher:sesame")
cat(getURL(url, userpwd = getOption("RDataCollectionLogin"), followlocation = TRUE))


### 9.1.7 Connections via HTTPS -----------------------------

u <- "https://www.icpsr.umich.edu/icpsrweb/ICPSR/ssvd/search"
u_action <- "https://www.icpsr.umich.edu/icpsrweb/ICPSR/ssvd/variables?"

# failure
u <- "https://www.icpsr.umich.edu/icpsrweb/ICPSR/ssvd/search"
getURL(u)

# success
signatures = system.file("CurlSSL", "cacert.pem", package = "RCurl")
res <- getURL(u, cainfo = signatures)

# alternative
res <- getURL(u, ssl.verifypeer = FALSE)

# get form, extract results
u_action <- "https://www.icpsr.umich.edu/icpsrweb/ICPSR/ssvd/variables?"
handle <- getCurlHandle(cainfo = signatures)
res <- getForm(u_action, variableLabel="climate+change", questionText="", categoryLabel="", curl = handle)
str_extract(res, "Your query returned [[:digit:]]+ variables")


### 9.1.8 Using cookies --------------------------------

# preparations
info   <- debugGatherer()
handle <- getCurlHandle(cookiejar      = "", 
                           followlocation = TRUE, 
                           autoreferer    = TRUE,
                           debugfunc      = info$update,
                           verbose        = TRUE,
                           httpheader     = list(
                             from         = "eddie@r-datacollection.com",
                             'user-agent' = str_c(R.version$version.string, 
                                              ", ", R.version$platform)
                           ))


# define urls						   
search_url <- "www.biblio.com/search.php?keyisbn=data"
cart_url   <- "www.biblio.com/cart.php"

# download and parse page
search_page <- htmlParse(getURL(url = search_url, curl = handle))

# identify form fields
xpathApply(search_page, "//div[@class='order-box'][position()<2]/form")

# extract book ids
xpath <- "//div[@class='order-box'][position()<4]/form/input[@name='bid']/@value"
bids <- unlist(xpathApply(search_page, xpath, as.numeric))
bids

# add items to shopping cart
for(i in seq_along(bids))  {
	res <- getForm(uri = cart_url, 
	                 curl = handle, 
	                 bid = bids[i], 
	                 add = 1, 
	                 int = "keyword_search")
}

# inspect shopping cart
cart  <- htmlParse(getURL(url=cart_url, curl=handle))
clean <- function(x)  str_replace_all(xmlValue(x),"(\t)|(\n\n)","")
cat(xpathSApply(cart, "//div[@class='title-block']", clean))

# request header
cat(str_split(info$value()["headerOut"],"\r")[[1]][1:13])

# response header
cat(str_split(info$value()["headerIn"],"\r")[[1]][1:14])



### 9.1.10 Retrieving data from APIs ---------------------------

# set local directory
setwd("scenario-apis")

# REST API: Yahoo Weather RSS Feed
# http://developer.yahoo.com/weather/

feed_url <- "http://weather.yahooapis.com/forecastrss"
feed <- getForm(feed_url , .params = list(w = "2422673", u = "c"))
parsed_feed <- xmlParse(feed)

# get current conditions
xpath <- "//yweather:location|//yweather:wind|//yweather:condition"
conditions <- unlist(xpathSApply(parsed_feed, xpath, xmlAttrs))

# get forecast
location <- t(xpathSApply(parsed_feed, "//yweather:location", xmlAttrs))
forecasts <- t(xpathSApply(parsed_feed, "//yweather:forecast", xmlAttrs))
forecast <- merge(location, forecasts)


# get WOEID with Yahoo Places API
#Yahoo ID: rdatacollection
#options(yahooid = "insert string here")
options(yahooid = "v.m4rTvV34GgKVVL5PEAG1uIcHyKfmY8mCJjqSl7Gx3Jkp3s2B14xCc89rQYKOmN8nc.OFbL")

baseurl <- "http://where.yahooapis.com/v1/places.q('%s')"
woeid_url <- sprintf(baseurl, URLencode("Hoboken, NJ, USA")) # careful: URL encoding!
parsed_woeid <- xmlParse((getForm(woeid_url, appid = getOption("yahooid"))))
woeid <- xpathSApply(parsed_woeid, "//*[local-name()='locality1']", xmlAttrs)[2,] # careful: namespaces!


# build wrapper function
getWeather <- function(place = "New York", ask = "current", temp = "c") {
	if (!ask %in% c("current","forecast")) {
		stop("Wrong ask parameter. Choose either 'current' or 'forecast'.")
	}
	if (!temp %in%  c("c", "f")) {
		stop("Wrong temp parameter. Choose either 'c' for Celsius or 'f' for Fahrenheit.")
	}	
## get woeid
	base_url <- "http://where.yahooapis.com/v1/places.q('%s')"
	woeid_url <- sprintf(base_url, URLencode(place))
	parsed_woeid <- xmlParse((getForm(woeid_url, appid = getOption("yahooid"))))
	woeid <- xpathSApply(parsed_woeid, "//*[local-name()='locality1']", xmlAttrs)[2,]
## get weather feed
	feed_url <- "http://weather.yahooapis.com/forecastrss"
	parsed_feed <- xmlParse(getForm(feed_url, .params = list(w = woeid, u = temp)))
## get current conditions
	if (ask == "current") {
	xpath <- "//yweather:location|//yweather:condition"
	conds <- data.frame(t(unlist(xpathSApply(parsed_feed, xpath, xmlAttrs))))
	message(sprintf("The weather in %s, %s, %s is %s. Current temperature is %s°%s.", conds$city, conds$region, conds$country, tolower(conds$text), conds$temp, toupper(temp)))
	}
## get forecast	
	if (ask == "forecast") {
	location <- data.frame(t(xpathSApply(parsed_feed, "//yweather:location", xmlAttrs)))
	forecasts <- data.frame(t(xpathSApply(parsed_feed, "//yweather:forecast", xmlAttrs)))
	message(sprintf("Weather forecast for %s, %s, %s:", location$city, location$region, location$country))
	return(forecasts)
	}
}

getWeather(place = "Paris", ask = "current", temp = "c")
getWeather(place = "Moscow", ask = "current", temp = "c")
getWeather(place = "Bern", ask = "forecast", temp = "c")



### 9.1.11 Authentication with OAuth ---------------------------

library(httr)

# Facebooks OAuth endpoints
facebook <- oauth_endpoint(
	authorize = "https://www.facebook.com/dialog/oauth",
	access = "https://graph.facebook.com/oauth/access_token")

# store consumer secret in the R environment
Sys.setenv(FACEBOOK_CONSUMER_SECRET = "3983746230hg8745389234...") # add consumer secret here
	
# bundly consumer key and secret
fb_app <- oauth_app("facebook", "485980054864321")
 
# set permissions
permissions <- "user_birthday, user_hometown, user_location, user_status,
user_checkins, friends_birthday, friends_hometown, friends_location,
friends_relationships, friends_status, friends_checkins, publish_actions,
read_stream, export_stream"
	
# ask for access credentials
fb_token <- oauth2.0_token(facebook, fb_app, scope = permissions, type =
"application/x-www-form-urlencoded")

# generate signature
fb_sig <- sign_oauth2.0(fb_token)


# use Rfacebook package to retrieve information from Facebook API
library(Rfacebook)
getUsers("hadleywickham", fb_sig, private_info = FALSE)

friends <- getFriends(fb_sig, simplify = TRUE)
nrow(friends)
table(friends_info$gender)
