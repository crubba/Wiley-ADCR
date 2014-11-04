### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 16: Gathering Data on Mobile Phones
### --------------------------------------------------------------



# --------------------------
### 16.4    Data storage ###
# --------------------------



# --------------------------------------
### 16.4.1    General considerations ###
# --------------------------------------


# packages
library(RSQLite)
library(stringr)


# load data
#load("dataFull/phones.Rdata")
load("phones.Rdata")

# establishing connection
sqlite  <- dbDriver("SQLite")
con     <- dbConnect(sqlite, "amazonProductInfo.db")



# ---------------------------------------------
### 16.4.2    Table definitions for storage ###
# ---------------------------------------------


# function for creating a ... table within the database
createPhones <- function(con){
    if(!dbExistsTable(con,"phones")){
    sql <- "CREATE TABLE phones (
                id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
                asin CHAR,
                UNIQUE(asin) ON CONFLICT IGNORE);"
         dbGetQuery(con, sql)
    }else{
        message("table already exists")
    }
}

# function for creating a ... table within the database
createProducers <- function(con){
    if(!dbExistsTable(con, "producers")){
        sql <- "CREATE TABLE producers (
                id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
                producer CHAR,
                UNIQUE(producer) ON CONFLICT IGNORE);"
         dbGetQuery(con, sql)
    }else{
        message("table already exists")
    }
}

# function for creating a ... table within the database
createModels <- function(con){
    if(!dbExistsTable(con, "models")){
        sql <- "CREATE TABLE models (
                id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
                model CHAR,
                UNIQUE(model) ON CONFLICT IGNORE);"
         dbGetQuery(con, sql)
    }else{
        message("table already exists")
    }
}

# function for creating a ... table within the database
createLinks <- function(con){
    if(!dbExistsTable(con, "links")){
        sql <- "CREATE TABLE links (
                id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
                link        TEXT);"
         dbGetQuery(con, sql)
    }else{
        message("table already exists")
    }
}

# function for creating a ... table within the database
createItems <- function(con){
    if(!dbExistsTable(con,"items")){
        sql <- "CREATE TABLE items (
                id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
                price REAL, 
                stars REAL, 
                rank INTEGER, 
                title TEXT,
                fname TEXT,
                time TEXT, 
                phones_id     INTEGER NOT NULL REFERENCES phones(id)    
                              ON UPDATE CASCADE, 
                producers_id  INTEGER NOT NULL REFERENCES producers(id) 
                              ON UPDATE CASCADE, 
                models_id     INTEGER NOT NULL REFERENCES models(id)    
                              ON UPDATE CASCADE, 
                links_id      INTEGER NOT NULL REFERENCES links(id)     
                              ON UPDATE CASCADE, 
                UNIQUE(
                  price, stars, rank, time, 
                  phones_id, producers_id, models_id, links_id
                ) ON CONFLICT IGNORE);"
         dbGetQuery(con, sql)
    }else{
        message("table already exists")
    }
}



# ---------------------------------------------------
### 16.4.3    Table de?nitions for future storage ###
# ---------------------------------------------------


# function for creating a ... table within the database
createReviews <- function(con){
if(!dbExistsTable(con,"reviews")){
        sql <- "CREATE TABLE reviews (
                id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
                asin        TEXT,
                stars       INTEGER,
                helpfulyes  INTEGER,
                helpfulno   INTEGER,
                helpfulsum  INTEGER,
                date        TEXT,
                title       TEXT,
                text        TEXT,
                UNIQUE(asin, stars, date, title, text) ON CONFLICT IGNORE);"
         dbGetQuery(con, sql)
    }else{
        message("table already exists")
    }
}

# function for creating a ... table within the database
createReviewsMeta <- function(con){
    if(!dbExistsTable(con,"reviewsMeta")){
        sql <- "CREATE TABLE reviewsMeta (
                id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
                asin      TEXT,
                one       INTEGER,
                two       INTEGER,
                three     INTEGER,
                four      INTEGER,  
                five      INTEGER,
                UNIQUE(asin) ON CONFLICT REPLACE);"
         dbGetQuery(con, sql)
    }else{
        message("table already exists")
    }
}





# ----------------------------------------------------------
### 16.4.4    View de?nitions for convenient data access ###
# ----------------------------------------------------------


# function for creating a ... view within the database
createViewItemData <- function(con){
    if(!dbExistsTable(con,"ItemData")){
        sql <- "CREATE VIEW ItemData AS
                SELECT items.id as itemid, price as itemprice, 
                stars as itemstars, rank as itemrank, 
                time as itemts, fname as itemfname,
                title as itemtitle, model, phones.asin as asin, 
                producer from items
                JOIN producers on producers_id = producers.id 
                JOIN models    on models_id    = models.id
                Join phones    on phones_id    = phones.id;"
         dbGetQuery(con, sql)
    }else{
        message("table already exists")
    }
}

# function for creating a ... view within the database
createViewReviewData <- function(con){
    if(!dbExistsTable(con,"ReviewData")){
        sql <- "CREATE VIEW ReviewData AS
                SELECT phones.asin, reviews.id as reviewid, 
                stars as reviewstars, one as allrev_onestar, 
                two as allrev_twostar, three as allrev_threestar, 
                four as allrev_fourstar, five as allrev_fivestar,
                helpfulyes, helpfulno, helpfulsum, date as reviewdate, 
                title as reviewtitle, text as reviewtext
                FROM phones 
                JOIN reviews on phones.asin=reviews.asin
                JOIN reviewsMeta on phones.asin=reviewsMeta.asin;"
         dbGetQuery(con, sql)
    }else{
        message("table already exists")
    }
}

# function for creating a ... view within the database
createViewAllData <- function(con){
    if(!dbExistsTable(con,"AllData")){
        sql <- "CREATE VIEW AllData AS
                SELECT * FROM ItemData
                JOIN ReviewData on ItemData.asin = ReviewData.asin"
         dbGetQuery(con, sql)
    }else{
        message("table already exists")
    }
}




# wrapper function for defining database scheme 
defineDatabase <- function(con){
    createPhones(con)
    createProducers(con)
    createModels(con)
    createLinks(con)
    createItems(con)
    createReviews(con)
    createReviewsMeta(con)
    createViewItemData(con)
    createViewReviewData(con)
    createViewAllData(con)
}


# convenience development function for dropping all tables
dropAll <- function(con){
    sql <- "select 'drop table ' || name || ';' from sqlite_master
    where type = 'table';"
    tmp <- grep("sqlite_sequence",unlist(dbGetQuery(con,sql)), 
            value=T,invert=T)
    for(i in seq_along(tmp)) dbGetQuery(con,tmp[i])
    sql <- "select 'drop view ' || name || ';' from sqlite_master
    where type = 'view';"
    tmp <- grep("sqlite_sequence",unlist(dbGetQuery(con,sql)), 
            value=T,invert=T)
    for(i in seq_along(tmp)) dbGetQuery(con,tmp[i])
}
# dropAll(con)





# ------------------------------------------
### 16.4.5    Functions for storing data ###
# ------------------------------------------

# add unique asins not already in DB
addASINs <- function(x, con){
    message("adding phones ...")
    asinsInDB  <- unlist(dbReadTable(con,"phones")["asin"])
    asinsToAdd <- unique(x$asins[!(x$asins %in% asinsInDB)])
    for(i in seq_along(asinsToAdd)){
        sql <- str_c("INSERT INTO phones (asin) VALUES ('", 
                     asinsToAdd[i], "') ;")
        dbGetQuery(con, sql)
    }
}

# add producers not already in DB
addProducers <- function(x, con){
    message("adding producers ...")
    producersInDB  <- unlist(dbReadTable(con,"producers")["producer"])
    producersToAdd <- unique(x$brands[!(x$brands %in% producersInDB)])
    for(i in seq_along(producersToAdd)){
        sql <- str_c("INSERT INTO producers (producer) VALUES ('",
                     producersToAdd[i], "') ;")
        dbGetQuery(con, sql)
    }
}

# add models not already in DB
addModels <- function(x, con){
    message("adding models ...")
    modelsInDB  <- unlist(dbReadTable(con,"models")["model"])
    modelsToAdd <- unique(x$model[!(x$model %in% modelsInDB)])
    for(i in seq_along(modelsToAdd)){
        sql <- str_c("INSERT INTO models (model) VALUES ('",
                    str_replace_all(modelsToAdd[i], "'", "''"),
                    "') ;")
        dbGetQuery(con, sql)
    }
}

# add links not already in DB
addLinks <- function(x, con){
    message("adding links ...")
    linksInDB  <- unlist(dbReadTable(con,"links")["link"])
    linksToAdd <- unique(x$link[!(x$link %in% linksInDB)])
    for(i in seq_along(linksToAdd)){
        sql <- str_c("INSERT INTO links (link) VALUES ('", linksToAdd[i], "') ;")
        dbGetQuery(con, sql)
    }
}


# add item data to DB 

addItems <- function(x, con){
    message("adding items ... ")
        # get fresh infos from db
        Phones     <-  dbReadTable(con, "phones")
        Producers  <-  dbReadTable(con, "producers")
        Models     <-  dbReadTable(con, "models")
        Links      <-  dbReadTable(con, "links")
    for(i in seq_along(x[,1])){
        priceDB     <- x$price[i]
        starsDB     <- x$stars[i]
        rankDB      <- x$rank[i]
        titleDB     <- str_replace(x$titles[i], "'", "''")
        fnameDB     <- x$fname[i]
        timeDB      <- x$timestamp[i]
        phonesDB <- Phones$id[Phones$asin %in% x$asin[i]]
        producersDB <- Producers$id[Producers$producer %in% x$brand[i]]
        modelsDB   <- Models$id[Models$model %in% x$model[i]]
        linksDB   <- Links$id[Links$link %in% x$link[i]]
        sql <- str_c(" INSERT INTO items 
                     (price, stars, rank, title, fname, time, 
                     phones_id, producers_id, models_id, links_id) 
                     VALUES 
                     ('",
                     str_c(  priceDB, starsDB, rankDB, titleDB, fnameDB, timeDB,
                             phonesDB, producersDB, modelsDB, linksDB, 
                             sep="', '"),
                 "'); ")
        dbGetQuery(con, sql)
    }
}

# function for saving data in database
saveInDatabase <- function(x, DBname){
    if(grepl("/dataFull$",getwd())) setwd("..")
    sqlite <- dbDriver("SQLite")
    con    <- dbConnect(sqlite, DBname)
    defineDatabase(con)
    addASINs(x, con)
    addProducers(x, con)
    addModels(x, con)
    addLinks(x, con)
    addItems(x, con)
    dbDisconnect(con)
}







# -------------------------------------------
### 16.4.6    Data storage and inspection ###
# -------------------------------------------


# usage of functions:
x <- phones[complete.cases(phones),]
saveInDatabase(x, "amazonProductInfo.db")

# test is data is in DB
dbReadTable(con, "phones")[1:3,]
dbReadTable(con, "producers")[1:3,]
dbReadTable(con, "models")[1:3,]
dbReadTable(con, "links")[1:3,]
dbReadTable(con, "items")[1:3,]
dbReadTable(con, "itemData")[1:3,]













