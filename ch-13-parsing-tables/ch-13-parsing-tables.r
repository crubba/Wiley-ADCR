### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 13: PARSING INFORMATION FROM SEMI-STRUCTURED
###                  DOCUMENTS
### --------------------------------------------------------------


# packages
require(stringr)
require(RCurl)


# Downloading Information
# ------------------------------------------------------------------ # 

# path to data on ftp
ftp  <- "ftp://ftp.wcc.nrcs.usda.gov/data/climate/table/temperature/history/california/"

# gen folder for data
if(!file.exists("Data")) dir.create("Data")

# get list of files from ftp
filelist <- getURL(ftp, dirlistonly = TRUE )
filelist <- unlist(str_split(filelist,"\r\n"))
filelist <- filelist[!filelist==""]
filelist

# get those files that entail averge temperatures
filesavg <- str_detect(filelist,"tavg")
filesavg <- filelist[filesavg]

# build url
urlsavg <- str_c(ftp,filesavg)
urlsavg

# save average temperatures if not already dowloaded
for(i in 1:length(urlsavg)){
    print( c(i,"/",length(urlsavg)) )
    fname <- paste0("Data/",filesavg[i])
    if( !file.exists(fname) ){
        download.file(urlsavg[i], fname)
        Sys.sleep( rpois(1,1) + runif(1,0,1) )
    }
}


# Extracting Meta Information
# ------------------------------------------------------------------ # 

# read in files and clue them together line by line
# to one big textfile
txt <- character()
for(i in 1:length(filesavg)){
    txt <- c(txt, readLines(str_c("Data/",filesavg[i])))
    }

# make them one line with newlines as "\n" characters
txt <- paste(txt,collapse="\n")

# split text into year tables
txtparts <- unlist(str_split(txt, "----------\n"))

# cleansing
txtparts <- str_replace(txtparts,"\n\\*\\*\\*This data is provisional and subject to change.","")
txtparts <- str_replace(txtparts,"^\n","")
txtparts <- txtparts[txtparts!=""]



# get the year
year <- str_extract(txtparts,"[[:digit:]]{2}  Average Air Temperature")
year <- str_extract(year,"[[:digit:]]{2}")
year <- ifelse(year < 20, str_c(20,year), str_c(19,year))
year <- as.numeric(year)


# get station and name
station <- str_extract(txtparts, "Station : .+?\n")
station     <- str_replace_all(station, "(Station : )|(\n)", "")
station     <- str_split(station,", ")
id          <- sapply(station, '[', 1)
name        <- sapply(station, '[', 2)



# Extracting Temperature Data and putting things together
# ------------------------------------------------------------------ # 

                                                start <- proc.time()
# extract part of the sections that contains daily temperatures
temperatures <- str_extract(txtparts, "day.*") 

# prepare object to store temperature data
tempData <- data.frame(avgtemp=NA, day=NA, month=NA, year=NA, id="", name="")

# generate a day pattern matching the order of temperatures
day      <- rep(1:31, 12)
# generate a month pattern matching the order of temperatures
month    <- rep( c(10:12,1:9), each=31 ) 

if(F==T){
# loop through all parts to get temperatures
for(i in seq_along(txtparts)){
    # write fixed width table into temporary file
    tf <- tempfile()
    writeLines(temperatures[i], tf)
    # read in data and transform to data frame
    temptable <- read.fwf(tf, width=c(3,7,6,6,6,6,6,6,6,6,6,6,6), stringsAsFactors=F)
    # keep only those lines and rows entailing day-temperatures 
    temptable <- temptable[3:33, -1]
    # transform data frame of strings to vector of type numeric
    temptable <- suppressWarnings(as.numeric(unlist(temptable)))
    # combine data
    temptable <- data.frame( avgtemp=temptable, day=day,      month=month, 
                             year=year[i],      name=name[i], id=id[i]     )
    # add data to tempData
    tempData <- rbind(tempData, temptable)
}
                                                proc.time() - start
}





# Building Parser
# ------------------------------------------------------------------ # 

parseTemp <- function(filename){
    # get text
    txt <- paste( readLines(filename), collapse="\n")
    # split text into year tables
    txtparts <- unlist(str_split(txt, "----------\n"))
    # cleansing
    txtparts <- str_replace(txtparts,
                            "\n\\*\\*\\*This data is provisional and subject to change.","")
    txtparts <- str_replace(txtparts,"^\n","")
    txtparts <- txtparts[txtparts!=""]
    # get the year
    year <- str_extract(txtparts,"[[:digit:]]{2}  Average Air Temperature")
    year <- str_extract(year,"[[:digit:]]{2}")
    year <- ifelse(year < 20, str_c(20,year), str_c(19,year))
    year <- as.numeric(year)
    # get station and name
    station <- str_extract(txtparts, "Station : .+?\n")
    station     <- str_replace_all(station, "(Station : )|(\n)", "")
    station     <- str_split(station,", ")
    id          <- sapply(station, '[', 1)
    name        <- sapply(station, '[', 2)
    # extract part of the sections that contains daily temperatures
    temperatures <- str_extract(txtparts, "day.*") 
    # prepare object to store temperature data
    tempData <- data.frame(avgtemp=NA, day=NA, month=NA, year=NA, id="", name="")
    # generate a day pattern matching the order of temperatures
    day      <- rep(1:31, 12)
    # generate a month pattern matching the order of temperatures
    month    <- rep( c(10:12,1:9), each=31 ) 
    # helper function
    doTemp <- function(temperatures, year, name, id){
        # write fixed width table into temporary file
        tf <- tempfile()
        writeLines(temperatures, tf)
        # read in data and transform to data frame
        temptable <- read.fwf(tf, width=c(3,7,6,6,6,6,6,6,6,6,6,6,6), 
                              stringsAsFactors=F)
        # keep only those lines and rows entailing day-temperatures 
        temptable <- temptable[3:33, -1]
        # transform data frame of strings to vector of type numeric
        temptable <- suppressWarnings(as.numeric(unlist(temptable)))
        # combine data
        temptable <- data.frame( avgtemp=temptable, day=day, month=month, 
                                 year=year, name=name, id=id )
        # add data to tempData
        tempData <<- rbind(tempData, temptable)
    }
    mapply(doTemp, temperatures, year, name, id)
    tempData <- tempData[!is.na(tempData$avgtemp),]
    return(tempData)
}

# wrapper function for parsing multiple files
parseTemps <- function(filenames){
    tmp <- lapply(filenames, parseTemp)
    tempData <- NULL
    for(i in seq_along(tmp)) tempData <- rbind(tempData,tmp[[i]])
    return(tempData)
}

tempData <- parseTemps( str_c("Data/",filesavg) )



# some further data on whether stations
# ------------------------------------------------------------------ # 

# download information
download.file("ftp://ftp.wcc.nrcs.usda.gov/states/ca/jchen/CA_sites.dat",
              "Data/CA_sites.dat")

# read in information
stationData <- read.csv("data/CA_sites.dat", header=F, sep="|")[,-c(1,2,7:9)]
names(stationData) <- c("name","lat","lon","alt")

# some data corrections
stationData$lon <- stationData$lon * -1
stationData[,c("lat","lon")] <- stationData[,c("lat","lon")]/100
stationData$alt <- stationData$alt / 3.2808399 # foot to meter
stationData <- stationData[order(stationData$lat),]

# a map of stations
# ------------------------------------------------------------------ # 



# packages
library(RgoogleMaps)



# downloading maps 
center  <- c(mean(range(stationData$lat)),mean(range(stationData$lon)))
map1 <- GetMap(
        destfile = "map1.png",  
        zoom=7,
        size=c(640,500),
        GRAYSCALE=T,
        center=center,
        maptype="hybrid",
        NEWMAP=FALSE)

map2 <- GetMap.OSM(
        latR=c(37.5,42),
        lonR=c(-125,-115),
        scale=5000000,
        destfile = "map2.png",
        GRAYSCALE=TRUE,
        NEWMAP=FALSE)
tmp <- readPNG("map2.png", native = FALSE)
tmp <- RGB2GRAY(tmp)
writePNG(tmp, "map2.png")

        
# plotting on maps

png("stationmap1.png",
    width=dim(readPNG("map1.png"))[2],
    height=dim(readPNG("map1.png"))[1])
PlotOnStaticMap(map1, 
                lat = stationData$lat, 
                lon = stationData$lon, 
                cex=2, pch=19,
                col=rgb(1,1,1,0.8), add=FALSE)
dev.off()

png("stationmap2.png",
    width=dim(readPNG("map2.png"))[2],
    height=dim(readPNG("map2.png"))[1])
PlotOnStaticMap(map2, 
                lat = stationData$lat, 
                lon = stationData$lon, 
                cex=2, pch=19,
                col=rgb(0,0,0,0.5), add=FALSE)
dev.off()

# some temperature plots
# ------------------------------------------------------------------ # 

# create time/date variable
#tempData$time <- as.numeric(str_c(tempData$year,
#                       str_pad(tempData$month,width=2,pad=0),
#                       str_pad(tempData$day,width=2,pad=0) ))
#tempData$date <- as.Date(strptime(tempData$time, "%Y%m%d")) 
#tempData <- tempData[order(tempData$date), ] 

# aggregate monthly data
monthlyTemp <- aggregate(tempData$avgtemp, by=list(name=tempData$name,month=tempData$month), mean)

# select stations to plot
stationNames <- c(  "ADIN MTN", "INDEPENDENCE CAMP", "SQUAW VALLEY G.C.",
                    "SPRATT CREEK", "LEAVITT MEADOWS","POISON FLAT")
stationAlt   <- stationData[match(stationNames, stationData$name),]$alt
stationLat   <- stationData[match(stationNames, stationData$name),]$lat
stationLon   <- stationData[match(stationNames, stationData$name),]$lon

# plot-function to apply on stations
plotTemps <- function(i){
iffer <- monthlyTemp$name==stationNames[i]
    plot( monthlyTemp[iffer, c("month","x")],
          type="b", 
          main=str_c(stationNames[i],
                     " (",round(stationAlt[i]),
                     "m)", 
                     "\n Lat.= ", stationLat[i],
                     " Lon.= ",   stationLon[i]),
          ylim=c(-15,25),
          ylab="average temperature" )
    abline(h=0,lty=2)
    iffer2 <- tempData$name==stationNames[i]
    points( tempData$month[iffer2] + tempData$day[iffer2] *0.032, 
            jitter(tempData$avgtemp[iffer2],3), col=rgb(0.2,0.2,0.2,0.1),pch=".")
}

# produce plot
pdf("stationstemp.pdf", width=9, height=6)
par(mfrow=c(2,3)) 
for(i in seq_along(stationNames)) plotTemps(i)
par(mfrow=c(1,1)) 
dev.off()
















