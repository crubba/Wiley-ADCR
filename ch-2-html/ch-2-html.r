### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 2: HTML
### --------------------------------------------------------------



### 2.4 Parsing
### --------------------------------------------------------------


#Reading in the HTML Code
url <- "http://www.r-datacollection.com/materials/html/fortunes.html"fortunes <- readLines(con = url)

#Parsing the Code using htmlParse
library(XML)parsed_fortunes <- htmlParse(file = url)print(parsed_fortunes)


### 2.4.2 Discarding Nodes
### --------------------------------------------------------------

#Using a handler function for discarding the <body> node and its children
h1 <- list("body" = function(x){NULL})
parsed_fortunes <- htmlTreeParse(url, handlers = h1, asTree = TRUE)parsed_fortunes$children


#Using generic handlers for DOM manipulation
h2 <- list(
startElement = function(node, ...){
	name <- xmlName(node)
	if(name %in% c("div", "title")){NULL}else{node}
	},
	comment = function(node){NULL}
)


parsed_fortunes <- htmlTreeParse(file = url, handlers = h2, asTree = TRUE)
parsed_fortunes$children


### 2.4.3 Extracting information in the building process
### --------------------------------------------------------------

#Specifying a function for generating handlers for the i nodes
getItalics = function() {
	i_container = character()
	list(i = function(node, ...){
		i_container <<- c(i_container, xmlValue(node))
		},
	returnI = function() i_container)}

#Instantiating an instance of the handler
h3 <- getItalics()

#Executing the parsing with the instatiated handler and printing the returned data
invisible(htmlTreeParse(url, handlers = h3))
h3$returnI()