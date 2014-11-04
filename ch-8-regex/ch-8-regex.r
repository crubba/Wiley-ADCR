### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 8: REGULAR EXPRESSIONS AND ESSENTIAL STRING FUNCTIONS
### --------------------------------------------------------------

# load packages
library(stringr)
library(XML)
library(RCurl)
library(tau)

# A difficult example
raw.data <- "555-1239Moe Szyslak(636) 555-0113Burns, C. Montgomery555-6542Rev. Timothy Lovejoy555 8904Ned Flanders636-555-3226Simpson, Homer5543642Dr. Julius Hibbert"

# Extract information
name <- unlist(str_extract_all(raw.data, "[[:alpha:]., ]{2,}"))
name
phone <- unlist(str_extract_all(raw.data, "\\(?(\\d{3})?\\)?(-| )?\\d{3}(-| )?\\d{4}"))
phone
data.frame(name = name, phone = phone)

### 8.1 Regular expressions
### --------------------------------------------------------------

# A running example
example.obj <- "1. A small sentence. - 2. Another tiny sentence."

### 8.1.1 Exact character matching
### --------------------------------------------------------------

# A string matching itself
str_extract(example.obj, "small")
str_extract(example.obj, "banana")
unlist(str_extract_all(example.obj, "sentence"))

# String vector
out <- str_extract_all(c("text", "manipulation", "basics"), "a")
out

# Case-sensitive matching
str_extract(example.obj, "small")
str_extract(example.obj, "SMALL")
str_extract(example.obj, ignore.case("SMALL"))

# Matching a particle, alphabetic characters and blank spaces
unlist(str_extract_all(example.obj, "en"))
str_extract(example.obj, "mall sent")

# Matching beginning and end of a string
str_extract(example.obj, "2")
str_extract(example.obj, "^2")
unlist(str_extract_all(example.obj, "sentence$"))

# Pipe operator
unlist(str_extract_all(example.obj, "tiny|sentence"))

### 8.1.2 Generalizing regular expressions
### --------------------------------------------------------------

# Any character
str_extract(example.obj, "sm.ll")

# Character classes
str_extract(example.obj, "sm[abc]ll")

# Ranges in character class
str_extract(example.obj, "sm[a-p]ll")

# Literal dots inside a character class
unlist(str_extract_all(example.obj, "[uvw. ]"))

# Predefined character classes
unlist(str_extract_all(example.obj, "[[:punct:]]"))

# Wrong usage of predefined character classes
unlist(str_extract_all(example.obj, "[:punct:]"))

# Redundancy inside a character class
unlist(str_extract_all(example.obj, "[AAAAA]"))

# Special characters inside a character class
str_extract("François Hollande", "Fran[a-z]ois")
str_extract("François Hollande", "Fran[[:alpha:]]ois")

# Adding to character classes
unlist(str_extract_all(example.obj, "[[:punct:]ABC]"))

# Negation of a character class
unlist(str_extract_all(example.obj, "[^[:alnum:]]"))

# Quantifiers
str_extract(example.obj, "s[[:alpha:]][[:alpha:]][[:alpha:]]")
str_extract(example.obj, "s[[:alpha:]]{3}l")

# Greedy matching
str_extract(example.obj, "A.+sentence")
str_extract(example.obj, "A.+?sentence")

# Quantifying a group of characters
unlist(str_extract_all(example.obj, "(.en){1,5}"))
unlist(str_extract_all(example.obj, ".en{1,5}"))

# Literal metacharacters
unlist(str_extract_all(example.obj, "\\."))
unlist(str_extract_all(example.obj, fixed(".")))

# Shortcuts of character classes
unlist(str_extract_all(example.obj, "\\w+"))

# Word edges
unlist(str_extract_all(example.obj, "e\\>"))
unlist(str_extract_all(example.obj, "e\\b"))

# Backreferencing
str_extract(example.obj, "([[:alpha:]]).+?\\1")
str_extract(example.obj, "(\\<[b-z]+\\>).+?\\1")

### 8.1.3 The introductory example reconsidered
### --------------------------------------------------------------

raw.data <- "555-1239Moe Szyslak(636) 555-0113Burns, C. Montgomery555-6542Rev. Timothy Lovejoy555 8904Ned Flanders636-555-3226Simpson, Homer5543642Dr. Julius Hibbert"

name <- unlist(str_extract_all(raw.data, "[[:alpha:]., ]{2,}"))
name
phone <- unlist(str_extract_all(raw.data, "\\(?(\\d{3})?\\)?(-| )?\\d{3}(-| )?\\d{4}"))
phone

### 8.2 String Processing
### --------------------------------------------------------------

### 8.2.1 The stringr package
### --------------------------------------------------------------

# str_extract()
str_extract(example.obj, "tiny")
str_extract_all(example.obj, "[[:digit:]]")

# str_locate() 
str_locate(example.obj, "tiny")

# str_sub()
str_sub(example.obj, start = 35, end = 38)
str_sub(example.obj, 35, 38) <- "huge"
example.obj

#str_replace()
str_replace(example.obj, pattern = "huge", replacement = "giant")

# str_split() / str_split_fixed()
unlist(str_split(example.obj, "-"))
as.character(str_split_fixed(example.obj, "[[:blank:]]", 5))

# A character vector
char.vec <- c("this", "and this", "and that")

# str_detect()
str_detect(char.vec, "this")

# str_count()
str_count(char.vec, "this")
str_count(char.vec, "\\w+")

# str_dup()
dup.obj <- str_dup(char.vec, 3)
dup.obj

# str_length()
length.char.vec <- str_length(char.vec)
length.char.vec

# str_pad() / str_trim()
char.vec <- str_pad(char.vec, width = max(length.char.vec), side = "both", pad = " ")
char.vec
char.vec <- str_trim(char.vec)
char.vec

# str_c()
cat(str_c(char.vec, collapse = "\n"))
str_c("text", "manipulation", sep = " ")
str_c("text", c("manipulation", "basics"), sep = " ")

### 8.2.2 A couple more handy functions
### --------------------------------------------------------------

# Approximate matching
agrep("Barack Obama", "Barack H. Obama", max.distance = list(all = 3))
agrep("Barack Obama", "Michelle Obama", max.distance = list(all = 3))

# Partial string matching
pmatch(c("and this", "and that", "and these", "and those"), char.vec)

# Make unique
make.unique(c("a", "b", "a", "c", "b", "a"))

### Extending base functionality - grepall()
# load Simpsons data set
load("episodes.Rdata")

grep("Homer", episodes$title[1:10], value = T)
grepl("Homer", episodes$title[1:10])

iffer1 <- grepl("Homer", episodes$title)
iffer2 <- grepl("Lisa", episodes$title)
iffer <- iffer1 & iffer2
episodes$title[iffer]

grepall <- function(pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE, value = FALSE, logic = FALSE){
    # error and exception handling
    if(length(pattern) == 0 | length(x) == 0){
        warning("Length of pattern or data equals zero.")
        return(NULL)
    }
    # apply grepl() and all()
    indices <- sapply(pattern, grepl, x, ignore.case, perl, fixed, useBytes)
    index <- apply(indices, 1, all)
    # indexation and return of results
    if(logic == T) return(index)
    if(value == F) return((1:length(x))[index])
    if(value == T) return(x[index])
}

grepall(c("Lisa", "Homer"), episodes$title)
grepall(c("Lisa", "Homer"), episodes$title, value = T)

### 8.3 A word on character encodings
### --------------------------------------------------------------

# Query your locale
Sys.getlocale()

# An example string
small.frogs <- "Små grodorna, små grodorna är lustiga att se."
small.frogs

# Translate the encoding
small.frogs.utf8 <- iconv(small.frogs, from = "windows-1252", to = "UTF-8")
small.frogs.utf8

# Declare an encoding
Encoding(small.frogs.utf8) <- "windows-1252"
small.frogs.utf8

# Sample from the list of available conversions
sample(iconvlist(), 10)

# Meta tags from Science
enc.test <- getURL("http://www.sciencemag.org/")
unlist(str_extract_all(enc.test, "<meta.+?>"))

# Guess encodings
is.locale(small.frogs)
is.ascii(small.frogs)
