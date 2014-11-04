namesPlot <- function(geodf, phonename, show.map = TRUE, save.pdf = TRUE,
                             minsize.cities = 450000, add.cities = "",
                             print.names = FALSE) {
## load libraries
	x <- c("stringr", "maptools", "rgdal", "maps")
	lapply(x, require, character.only = TRUE)
## prepare coordinates
	coords <- SpatialPoints(cbind(geodf$lon, geodf$lat))
	proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
## prepare map
	projection <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
	map_germany <- readShapePoly("function_data/DEU_adm0.shp",
                                     proj4string = projection)
	map_germany_laender <- readShapePoly("function_data/DEU_adm1.shp",
                                             proj4string = projection)
## add big cities (from maps package)
	data("world.cities")
	cities_ger <- subset(world.cities, country.etc == "Germany" &
                      (world.cities$pop > minsize.cities |
                      world.cities$name %in% add.cities))
	coords_cities <- SpatialPoints(cbind(cities_ger$long, cities_ger$lat))
## produce map
	i <- 0
	n <- 1
	while (i <= n) {
		if (save.pdf == TRUE & i < n) {
			pdf(file = str_c("phonebook_", phonename, "/map-",
                phonename, ".pdf"), height = 10, width = 7.5,
                family = "URWTimes")
		}
		if (show.map == TRUE | save.pdf == TRUE) {
			par(oma = c(0, 0, 0, 0))
			par(mar = c(0, 0, 2, 0))
			par(mfrow = c(1, 1))
			plot(map_germany)
			plot(map_germany_laender, add = TRUE)
			title(main = str_c("People named ", toupper(phonename),
                        " in Germany"))
		if (print.names == FALSE) {
			points(coords$coords.x1, coords$coords.x2,
                   col = rgb(10, 10, 10, max = 255), 
                   bg = rgb(10, 10, 10, max = 255),
                   pch = 20, cex = 1)
		} else {
			text(coords$coords.x1, coords$coords.x2,
  			     labels = str_replace_all(geodf$name, 
				 ignore.case(phonename), ""), cex = .7)
		}
		points(coords_cities, col = "black", , bg = "grey", pch = 23)
		shadowtext(cities_ger$long,cities_ger$lat, 
		           labels = cities_ger$name, pos = 4, 
				   col = "black", bg = "white", cex =  1.2)
		}
		if (save.pdf == TRUE & i < n) {
			dev.off()
		}
		if (show.map == FALSE) {
			i <- i + 1
		}
		i <- i + 1
	}
}