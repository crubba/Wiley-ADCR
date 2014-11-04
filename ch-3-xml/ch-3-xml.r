### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 3: XML AND JSON
### --------------------------------------------------------------

# load packages
library(XML)

# inspect XML package
packageVersion("XML")
ls("package:XML")
lsf.str("package:XML")


### 3.5 XML and R in Practice
### --------------------------------------------------------------

# import and parse XML file
parsed_stocks <- xmlParse(file = "stocks/technology.xml")

# validate XML
stocks <- xmlParse(file = "stocks/technology.xml", validate = TRUE)
stocks <- xmlParse(file = "stocks/technology-manip.xml", validate = TRUE)

# import and parse XML file
bond <- xmlParse("bond.xml")
class(bond)
bond

# basic operations
(root <- xmlRoot(bond))
xmlName(root)
xmlSize(root)

# rudimentary navigation in XMLs
root[[1]]
root[[1]][[1]]

root[["movie"]]
root["movie"]
root[["movie"]][[1]][[1]]

# accessing RSS files
xmlParse("rsscode.rss")

# basic content retrieval
xmlValue(root)
xmlValue(root[[1]])
xmlAttrs(root[[1]])
xmlGetAttr(root[[1]], "id")

xmlSApply(root, xmlValue)
xmlSApply(root[[1]], xmlValue)
xmlSApply(root, xmlAttrs)
xmlSApply(root, xmlGetAttr, "id")

# using xmlToDataFrame function
(movie.df <- xmlToDataFrame(root))
 
# using xmlToList function
(movie.list <- xmlToList(bond))
 

# XML event parsing

# create event handler
branchFun <- function(){
	container_close <- numeric()
	container_date <- numeric()
	
	Apple <- function(node,...) {
		date <- xmlValue(xmlChildren(node)[[c("date")]])
		container_date <<- c(container_date, date)
		close <- xmlValue(xmlChildren(node)[[c("close")]])
		container_close <<- c(container_close, close)
                #print(c(close, date));Sys.sleep(0.5)
    }
    getContainer <- function() data.frame(date=container_date, close=container_close)
    list(Apple=Apple, getStore=getContainer)
}

# generate instance of handler function
(h5 <- branchFun())

# generate instance of handler function
invisible(xmlEventParse(file = "stocks/technology.xml", branches = h5, handlers = list()))

# fetch Apple stock
apple.stock <- h5$getStore()
head(apple.stock, 5)
