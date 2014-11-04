namesScrape <- function(phonename, update.file = FALSE) {
## transform phonename
	phonename <- tolower(phonename)
## load libraries
	x <- c("stringr", "RCurl", "XML")
	lapply(x, require, character.only=T)
## create folder
	dir.create(str_c("phonebook_", phonename), showWarnings = FALSE)
	filename <- str_c("phonebook_", phonename, "/phonebook_", phonename, ".html")
	if (file.exists(filename) & update.file == FALSE) {
		message("Data already scraped; using data from ", file.info(filename)$mtime)
	} else {
## retrieve and save html
		tb <- getForm("http://www.dastelefonbuch.de/",
		.params = c(kw = phonename, cmd = "search", ao1 = "1", reccount = "2000"))
		write(tb, file = filename)
	}
}