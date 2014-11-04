### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 4: XPATH
### --------------------------------------------------------------


### 4.1 - 4.2 XPath - a Querying Language for Web Documents / Identifying Node Sets with XPath
### --------------------------------------------------------------

#load packages and set working directory
library(XML)

#Parsing and printing the HTML document
parsed_doc <- htmlParse(file = "fortunes/fortunes.html")
print(parsed_doc)

#Basic structural queries
xpathSApply(doc = parsed_doc, path = "/html/body/div/p/i")
xpathSApply(parsed_doc, "//body//p/i")
xpathSApply(parsed_doc, "//p/i")
xpathSApply(parsed_doc, "/html/body/div/*/i")
xpathSApply(parsed_doc, "//title/..")
xpathSApply(parsed_doc, "//address | //title")

twoQueries <- c(address = "//address", title = "//title")xpathSApply(parsed_doc, twoQueries)


### 4.2.2 Node relations
### --------------------------------------------------------------

#Queries that exploit a node's relationship
xpathSApply(parsed_doc, "//a/ancestor::div")
xpathSApply(parsed_doc, "//a/ancestor::div//i")
xpathSApply(parsed_doc, "//p/preceding-sibling::h1")
xpathSApply(parsed_doc, "//title/parent::*")


### 4.2.3 XPath predicates
### --------------------------------------------------------------

#Numerical and position predicates
xpathSApply(parsed_doc, "//div/p[position()=1]")
xpathSApply(parsed_doc, "//div/p[last()]")
xpathSApply(parsed_doc, "//div/p[last()-1]")
xpathSApply(parsed_doc, "//div[count(.//a)>0]")
xpathSApply(parsed_doc, "//div[count(./@*)>2]")
xpathSApply(parsed_doc, "//*[string-length(text())>50]")
xpathSApply(parsed_doc, "//div[not(count(./@*)>2)]")

#Exact mactching and predicates for string manipulation
xpathSApply(parsed_doc, "//div[@date='October/2011']")
xpathSApply(parsed_doc, "//*[contains(text(), 'magic')]")
xpathSApply(parsed_doc, "//div[starts-with(./@id, 'R')]")
xpathSApply(parsed_doc, "//div[substring-after(./@date, '/')='2003']//i")


### 4.3 Extracting Node Elements
### --------------------------------------------------------------

xpathSApply(parsed_doc, "//title", fun = xmlValue)
xpathSApply(parsed_doc, "//div", xmlAttrs)
xpathSApply(parsed_doc, "//div", xmlGetAttr, "lang")


### 4.3.1 Extending the fun argument
### --------------------------------------------------------------

#Convert capital letters from a node's value
lowerCaseFun <- function(x) {
	x <- tolower(xmlValue(x))
	return(x)
	}

xpathSApply(parsed_doc, "//div//i", fun = lowerCaseFun)


#Extract a string formatted like a year from a node's value
dateFun <- function(x) {    require(stringr)
    date <- xmlGetAttr(node = x, name = "date")
    year <- str_extract(date, "[0-9]{4}")
    return(year)
    }

xpathSApply(parsed_doc, "//div", dateFun)

#Replace NULL values returned from a query on the id attribute
idFun <- function(x) {
	id <- xmlGetAttr(x, "id")
	id <- ifelse(is.null(id), "not specified", id)
	return(id)
}

xpathSApply(parsed_doc, "//div", idFun)


### 4.3.1 Using variables in XPath expressions
### --------------------------------------------------------------
parsed_stocks <- xmlParse(file = "technology/technology.xml")
companies <- c("Apple", "IBM", "Google")

(expQuery <- sprintf("//%s/close", companies))

getClose <- function(node) {
	value <- xmlValue(node)
	company <- xmlName(xmlParent(node))
	mat <- c(company = company, value = value)
	return(mat)}

as.data.frame(t(xpathSApply(parsed_stocks, expQuery, getClose))
stocks$value <- as.numeric(as.character(stocks$value))head(stocks, 3)


### XML namespaces
### --------------------------------------------------------------
parsed_xml <- xmlParse("titles.xml")
parsed_xml

xpathSApply(parsed_xml, "//title", fun = xmlValue)
xpathSApply(parsed_xml, "//*[local-name()='title']", xmlValue)

xpathSApply(parsed_xml, "//x:title", namespaces = c(x = "http://funnybooknames.com/crockford"), fun = xmlValue)
xpathSApply(parsed_xml, "//x:title", namespaces = c(x = "http://www.w3.org/1999/xhtml"), fun = xmlValue)

nsDefs <- xmlNamespaceDefinitions(parsed_xml)[[2]]
ns <- nsDefs$uri
ns 

xpathSApply(parsed_xml, "//x:title", naemspaces = c(x = ns), xmlValue)
