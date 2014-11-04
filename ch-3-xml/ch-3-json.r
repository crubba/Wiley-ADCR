### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 3: XML AND JSON
### --------------------------------------------------------------

# load packages
library(RJSONIO)
library(stringr)
library(plyr)

# inspect package RJSONIO
packageVersion("RJSONIO")
ls("package:RJSONIO")
lsf.str("package:RJSONIO")


### 3.8 JSON and R in Practice
### --------------------------------------------------------------

# check validity of JSON document
isValidJSON("indy.json")

# parse object
indy <- fromJSON("indy.json")

# inspect object
class(indy)
names(indy)
indy
indy[[1]]
indy[[1]][[1]]


# extract values from object - imported objects are lists

# step-by-step approach 1
sapply(indy[[1]], '[[', "name")
sapply(indy[[1]], '[[', "actors")
sapply(indy[[1]], '[[', "year")

# step-by-step approach 2
library(stringr)
indy.vec <- unlist(indy, recursive = TRUE, use.names = TRUE)
indy.vec[str_detect(names(indy.vec), "name")]

# data frame approach
indy.unlist <- sapply(indy[[1]], unlist)
indy.df <- do.call("rbind.fill", lapply(lapply(indy.unlist, t), data.frame, stringsAsFactors = FALSE))
names(indy.df)
 

# Peanuts example
peanuts.json <- fromJSON('peanuts.json', nullValue=NA, simplify = FALSE)
peanuts.df <- do.call("rbind", lapply(peanuts.json, data.frame, stringsAsFactors = FALSE))
peanuts.df 

# export to JSON
peanuts.out.json <- toJSON(peanuts.df, pretty = TRUE)
file.output <- file("peanuts_out.json")
writeLines(peanuts.out.json, file.output)
close(file.output)


# working with the jsonlite package

# load jsonlite package
detach("package:RJSONIO", unload=TRUE)
library(jsonlite)

# inspect package jsonlite
packageVersion("jsonlite")
ls("package:jsonlite")
lsf.str("package:jsonlite")


x <- '[1, 2, true, false]' # numeric
fromJSON(x)
x <- '["foo", true, false]' # character
fromJSON(x)
x <- '[TRUE, true, false]' # logical
fromJSON(x)
x <- '[foo, true, false]' 
fromJSON(x)
x <- '[true, false, null, null]'
fromJSON(x)
x <- '["foo", null]'
fromJSON(x)
x <- '[true, false, null]'
fromJSON(x)
x <- '[1, "foo", null, false]'
fromJSON(x)
x <- '[true, false, true, ]'
fromJSON(x)
x <- '{"foo":[1,2]}'
fromJSON(x)
x <- '{"foo":1}'
fromJSON(x)

# Peanuts example reconsidered
(peanuts.json <- fromJSON('peanuts.json'))

# Indy example reconsidered
(indy <- fromJSON("indy.json"))
indy.df <- indy$`indy movies`
indy.df$name




