### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 15: MAPPING THE GEOGRAPHIC DISTRIBUTION OF NAMES
### --------------------------------------------------------------

# load packages
library(stringr)
library(RCurl)
library(XML)
library(maptools)
library(rgdal)
library(maps)
library(TeachingDemos) # shadowtext() function


# 1. Identify appropriate page: http://www.dastelefonbuch.de/
# 2. Develop a strategy:
	# a) entries can be requested with GET mechanism
	# b) a maximum of 2000 entries are displayed --> not enough for popular names!
	# c) search can be restricted with postcodes
	# d) strategy: construct a set of get entries and scrape every single page. what varies is the zipcode restriction:
# 3. Apply strategy: retrieve data, extract information, cleanse data, document practical problems
# 4. Visualize / analyze data
# 5. Generalize scraping task if the task has to be repeated over and over again


# retrieve HTML content
tb <- getForm("http://www.dastelefonbuch.de/", .params = c(kw = "Feuerstein", cmd = "search", ao1 = "1", reccount = "2000"))
dir.create("phonebook_feuerstein")
write(tb, file =  "phonebook_feuerstein/phonebook_feuerstein.html")

# parse HTML file
tb_parse <- htmlParse("phonebook_feuerstein/phonebook_feuerstein.html",encoding="UTF-8")

# retrieve number of results with XPath and regular expression
xpath <- '//ul/li/a[contains(text(), "Privat")]'
num_results <- xpathSApply(tb_parse, xpath, xmlValue)
num_results
num_results <- as.numeric(str_extract(num_results, '[[:digit:]]+'))
num_results

# extracting zipcodes and names separately
xpath <- '//div[@class="name"]/a[@title]'
surnames <- xpathSApply(tb_parse, xpath, xmlValue)
surnames[1:5]

xpath <- '//span[@itemprop="postal-code"]'
zipcodes <- xpathSApply(tb_parse, xpath, xmlValue)
zipcodes[1:5]

length(surnames)
length(zipcodes)

# Vectors have different lengths: a deeper look in the file shows that there are malformed zip codes and missing names. If we were only interested in the location of hits, we could drop the names and just refer to the zip codes. If we want to keep names and zipcodes together though, we have to adapt the extraction.

# extract names - but only those for which a zipcode is available!
xpath <- '//span[@itemprop="postal-code"]/ancestor::div[@class="popupMenu"]/preceding-sibling::div[@class="name"]'
names_vec <- xpathSApply(tb_parse, xpath, xmlValue)
names_vec <- str_replace_all(names_vec, "(\\n|\\t|\\r| {2,})", "")
names_vec[1:5]

# extract zip codes - but only those for which a name is available!
xpath <- '//div[@class="name"]/following-sibling::div[@class="popupMenu"]//span[@itemprop="postal-code"]'
zipcodes_vec <- xpathSApply(tb_parse, xpath, xmlValue)
zipcodes_vec <- as.numeric(zipcodes_vec)
zipcodes_vec[1:5]

# build data frame
entries_df <- data.frame(plz = zipcodes_vec, name = names_vec)
head(entries_df)

# get and match geo coordinates
# get list of zip codes with coordinates from OpenGeoDB:
# http://opengeodb.org/wiki/OpenGeoDB_Downloads
# http://fa-technik.adfc.de/code/opengeodb/PLZ.tab

dir.create("geo_germany")
download.file("http://fa-technik.adfc.de/code/opengeodb/PLZ.tab", destfile="geo_germany/plz_de.txt")
plz_df <- read.delim("geo_germany/plz_de.txt", stringsAsFactors=FALSE,encoding="UTF-8")
head(plz_df)
places_geo <- merge(entries_df, plz_df, by="plz", all.x = TRUE)
head(places_geo)

# get map 
# Download free map from http://www.gadm.org/country
#"The coordinate reference system is latitude/longitude and theWGS84 datum."
download.file("http://biogeo.ucdavis.edu/data/gadm2/shp/DEU_adm.zip", destfile="geo_germany/ger_shape.zip")
unzip("geo_germany/ger_shape.zip", exdir = "geo_germany")
dir("geo_germany")

# read geodata and set projection attributes for map
projection <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
map_germany <- readShapePoly(paste(getwd(),"/geo_germany/DEU_adm0.shp", sep=""), proj4string=projection)
map_germany_laender <- readShapePoly(paste(getwd(),"/geo_germany/DEU_adm1.shp", sep=""), proj4string=projection)

# set projection attributes for coordinates
coordinates <- SpatialPoints(cbind(places_geo$lon,places_geo$lat))
proj4string(coordinates) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# add big cities (from maps package)
data("world.cities")
cities_ger <- world.cities[world.cities$country.etc == "Germany" & (world.cities$pop > 460000 | world.cities$name =="Mannheim"  | world.cities$name =="Jena"),]
coords_cities <- SpatialPoints(cbind(cities_ger$long,cities_ger$lat))

# plot map
dir.create("figures")
#pdf(file="figures/map-feuerstein.pdf", height=10, width=7.5, family="URWTimes")
par(oma=c(0,0,0,0))
par (mar=c(0,0,0,0))
par(mfrow=c(1,1))
plot(map_germany)
plot(map_germany_laender, add = T)
points(coordinates$coords.x1, coordinates$coords.x2, col=rgb(10,10,10,max=255), bg=rgb(10,10,10,max=255), pch=20, cex=1)
points(coords_cities, col = "black", , bg="grey", pch=23)
shadowtext(cities_ger$long,cities_ger$lat, labels = cities_ger$name, pos = 4, col = "black", bg = "white", cex =  1.2)
#dev.off()
