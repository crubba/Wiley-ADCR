namesParse <- function(phonename) {
	filename <- str_c("phonebook_", phonename, "/phonebook_", phonename, ".html")
## load libraries
	x <- c("stringr", "XML")
	lapply(x, require, character.only = TRUE)
## parse html
	tb_parse <- htmlParse(filename, encoding = "UTF-8")
## check number of hits
	xpath <- '//ul/li/a[contains(text(), "Privat")]'
	num_results <- xpathSApply(tb_parse, xpath, xmlValue)
	num_results <- as.numeric(str_extract(num_results, '[[:digit:]]+'))
	if (num_results <= 2000) {
		message('Gross sample of ', num_results, ' entries retrieved.')
	}
	if (num_results > 2000) {
		message(num_results, ' hits. Warning: No more than 2,000 entries will be retrieved')
	}
## retrieve zipcodes and names
	xpath <- '//div[@class="name"]/following-sibling::div[@class="popupMenu"]//span[@itemprop="postal-code"]'
	zipcodes_vec <- xpathSApply(tb_parse, xpath, xmlValue)
	zipcodes_vec <- str_replace_all(zipcodes_vec, " ", "")
	xpath <- '//span[@itemprop="postal-code"]/ancestor::div[@class="popupMenu"]/preceding-sibling::div[@class="name"]'
	names_vec <- xpathSApply(tb_parse, xpath, xmlValue)
	names_vec <- str_replace_all(names_vec, "(\\n|\\t|\\r| {2,})", "")
## build data frame
	entries_df <- data.frame(plz = as.numeric(zipcodes_vec), name = names_vec)
## match coordinates to zipcodes
	plz_df <- read.delim("function_data/plz_de.txt", stringsAsFactors = FALSE, encoding = "UTF-8")
	geodf <- merge(entries_df, plz_df, by = "plz", all.x = TRUE)
	geodf <- geodf[!is.na(geodf$lon),]
## return data frame
	geodf <- geodf[,!names(geodf) %in% "X.loc_id"]
	return(geodf)
}