### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 12: COLLABORATION NETWORKS IN THE U.S. SENATE
### --------------------------------------------------------------

# load packages
library(RCurl)
library(XML)
library(httr)
library(stringr)
library(igraph)

### 12.1 Information on the Bills
### --------------------------------------------------------------

#Generate empty folder
dir.create("Bills_111/")

# Iterate over all 4059 pieces of legislation
for(i in 1:4059){
    # Generate the unique URL for each piece of legislation
    url <- str_c(
        "http://thomas.loc.gov/cgi-bin/bdquery/D?d111:", 
        i, 
        ":./list/bss/d111SN.lst:@@@P"
    )
    # Download the page
    bill.result <- getURL(url)
    # Write the page to local hard drive
    write(
        bill.result, str_c(
            "Bills_111/Bill_111_S", 
            i, 
            ".html"
        )
    )
    # Print progress of download
    cat(i, "\n")
}

# Create regular expressions to extract sponsors/cosponsors
sponsor.regex <- "FLD003\\+@4\\(\\(@1\\([[:alpha:]+.]+"
cosponsor.regex <- "FLD004\\+@4\\(\\(@1\\([[:alpha:]+.]+"

# Read data back in
html.source <- readLines("Bills_111/Bill_111_S1.html")
sponsor <- str_extract(html.source, sponsor.regex)
(sponsor <- sponsor[!is.na(sponsor)])
cosponsors <- unlist(str_extract_all(html.source, cosponsor.regex))
cosponsors[1:3]
length(cosponsors)

# Create clean up function
clean.up <- function(x){
    name <- str_extract(x, "[[:alpha:]+.]+$")
    name <- str_replace_all(name, fixed("++"), ", ")
    name <- str_replace_all(name, fixed("+"), " ")
    name <- str_trim(str_replace(name, "Sen", ""))    
    return(name)
}

# Apply clean up function
clean.up(sponsor)
clean.up(cosponsors)

# Extract all sponsors/cosponsors
error.collection <- list()
sponsor.list <- list()
# Iterate over all 4059 pieces of legislation
for(i in 1:4059){
    # Read the ith result
    html.source <- readLines(str_c("Bills_111/Bill_111_S", i, ".html"))
    # Extract and clean the sponsor
    sponsor <- unlist(str_extract_all(html.source, sponsor.regex))
    sponsor <- sponsor[!is.na(sponsor)]
    sponsor <- clean.up(sponsor)
    # Extract and clean the cosponsors
    cosponsors <- unlist(str_extract_all(html.source, cosponsor.regex))
    cosponsors <- clean.up(cosponsors)
    
    # Input the results into the sponsor list
    sponsor.list[[str_c("S.", i)]] <- list(sponsor = sponsor, cosponsors = cosponsors)
    #cat(i, "\n")
    
    # Collect potential points of error
    # Collect number of cosponsors
    fail.safe <- str_extract(html.source, "COSPONSORS?\\(([[:digit:]]{1,3}|S)\\)")
    fail.safe <- fail.safe[!is.na(fail.safe)]
    # Error - no cosponsor string
    if(length(fail.safe) == 0){
        #cat(i, "--:", "String - COSPONSOR - not found", "\n")
        error.collection[[length(error.collection) + 1]] <- c(i, "String - COSPONSOR - not found")
    }
    # Error - found more cosponsors than possible
    if(fail.safe == "COSPONSOR(S)"){
        if(length(cosponsors) > 0){
            #cat(i, "--:", "Found cosponsors where there should be none", "\n")
            error.collection[[length(error.collection) + 1]] <- c(i, "Found cosponsors where there should be none")
        }    
    }else{
        right.number <- str_extract(fail.safe, "[[:digit:]]+")
        # Error - Found wrong number of cosponsors
        if(length(cosponsors) != right.number){
            #cat(i, "--:", "Did not find the right number of cosponsors", "\n")
            error.collection[[length(error.collection) + 1]] <- c(i, "Did not find the right number of cosponsors")
        }
    }
    # Error - Found no sponsors 
    if(is.na(sponsor)){
        #cat(i, "--:", "No sponsors", "\n")
        error.collection[[length(error.collection) + 1]] <- c(i, "No sponsors")
    }
    # Error - Found too many sponsors
    if(length(sponsor) > 1){
        #cat(i, "--:", "More than one sponsor", "\n")
        error.collection[[length(error.collection) + 1]] <- c(i, "More than one sponsor")
    }
}
# Output length of error collection
length(error.collection)

# Correct for withdrawals
for(i in 1:length(error.collection)){
    bill.number <- as.numeric(error.collection[[i]][1])
    html.source <- readLines(str_c("Bills_111/Bill_111_S", bill.number, ".html"))
    
    count.withdrawn <- unlist(
        str_extract_all(
            html.source, 
            "\\(withdrawn - [[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}\\)"
        )
    ) 
    
    sponsor.list[[str_c("S.", bill.number)]]$cosponsors <- sponsor.list[[str_c("S.", bill.number)]]$cosponsors[1:(length(sponsor.list[[str_c("S.", bill.number)]]$cosponsors) - length(count.withdrawn))]
}

# Create a vector of all sponsors
all.senators <- unlist(sponsor.list)
all.senators <- unique(all.senators)
all.senators <- sort(all.senators)
head(all.senators)

# Create a matrix of sponsors
sponsor.matrix <- matrix(NA, nrow = 4059, ncol = length(all.senators))
colnames(sponsor.matrix) <- all.senators
rownames(sponsor.matrix) <- paste("S.", seq(1, 4059), sep ="")
sponsor.matrix[30:35, 31:34]

# Fill the matrix
for(i in 1:length(sponsor.list)){
    sponsor.matrix[i, which(all.senators == sponsor.list[[i]]$sponsor)] <- "Sponsor"
    if(length(sponsor.list[[i]]$cosponsors) > 0){
        for(j in 1:length(sponsor.list[[i]]$cosponsors)){
            sponsor.matrix[i, which(all.senators == sponsor.list[[i]]$cosponsors[j])] <- "Cosponsor"
        }
    }
}
sponsor.matrix[30:35,31:34]

### 12.2 Information on the Senators
### --------------------------------------------------------------

# Check out form for senator information extraction
form.page <- getURL("http://bioguide.congress.gov/biosearch/biosearch.asp")
write(form.page, "form_page.html")

# Check out elements of form
form.page <- str_c(readLines("form_page.html"), collapse = "")
destination <- str_extract(form.page, "<form.+?>")
cat(destination)
form <- str_extract(form.page, "<form.+?</form>")
cat(str_c(unlist(str_extract_all(form, "<INPUT.+?>")), collapse = "\n"))
cat(str_c(unlist(str_extract_all(form, "<SELECT.+?>")), collapse = "\n"))

# Download biographical information
senator.site <- POST(
    "http://bioguide.congress.gov/biosearch/biosearch1.asp", 
    body = list(
        lastname = "", 
        firstname = "", 
        position = "Senator", 
        state = "", 
        party = "", 
        congress = "111"
    ), 
    multipart = F
)
write(content(senator.site, as = 'text'), "senators.html")

# Put senator information in data frame
senator.site <- readLines("senators.html")
senator.site <- htmlParse(senator.site)
senators <- readHTMLTable(senator.site)[[2]]
head(senators)

# Match senators' names
senators$match.names <- senators[,1]
senators$match.names <- tolower(senators$match.names)
senators$match.names <- str_extract(senators$match.names, "[[:alpha:]]+")

all.senators.dat <- data.frame(all.senators)
all.senators.dat$match.names <- str_extract(all.senators.dat$all.senators, "[[:alpha:]]+")
all.senators.dat$match.names <- tolower(all.senators.dat$match.names)

senators <- merge(all.senators.dat, senators, by = "match.names")
senators[,2] <- as.character(senators[,2])
senators[,3] <- as.character(senators[,3])
senators[,2] <- tolower(senators[,2])
senators[,3] <- tolower(senators[,3])

# Correct for duplicates
allDup <- function(x){
    duplicated(x) | duplicated(x, fromLast = TRUE)
} 

dup.senators <- senators[allDup(senators[,1]),]
senators <- senators[rownames(senators) %in% rownames(dup.senators) == F,]

dup.senators[str_detect(dup.senators[,3], "\\("), 3] <- str_replace_all(dup.senators[str_detect(dup.senators[,3], "\\("), 3], ", .+?\\(", ", ")
dup.senators[str_detect(dup.senators[,3], "\\)"), 3] <- str_replace_all(dup.senators[str_detect(dup.senators[,3], "\\)"), 3], "\\)", "")

for(i in nrow(dup.senators):1){
    if(str_detect(dup.senators[i, 2], str_extract(dup.senators[i, 3], "[^,][[:alpha:] .]+?$")) == F){
        dup.senators <- dup.senators[-i,]
    }
}

senators <- rbind(senators, dup.senators)
senators$rownames <- as.numeric(rownames(senators))
senators <- senators[order(senators$rownames),]
dim(senators)

# Replace column heads
colnames(sponsor.matrix) <- senators$all.senators

### 12.3 Analyzing the network structure
### --------------------------------------------------------------

# Create edge list
edgelist.sponsors <- matrix(NA, nrow = 0, ncol = 2)
for(i in 1:nrow(sponsor.matrix)){
    if(length(which(!is.na(sponsor.matrix[i,]))) > 1){
        edgelist.sponsors <- rbind(
            edgelist.sponsors, 
            t(combn(colnames(sponsor.matrix)[which(!is.na(sponsor.matrix[i,]))], 2))
        )
    }
}

# Create graph object
sponsor.network <- graph.edgelist(edgelist.sponsors, directed = F)

# Sponsorship count
result <- matrix(
    NA, 
    ncol = ncol(sponsor.matrix), 
    nrow = 2, 
    dimnames = list(
        c("Sponsor", "Cosponsor"), 
        colnames(sponsor.matrix)
    )
)
for(i in 1:ncol(sponsor.matrix)){
    result[1, i] <- sum(sponsor.matrix[, i] == "Cosponsor", na.rm = T)
    result[2, i] <- sum(sponsor.matrix[, i] == "Sponsor", na.rm = T)
}
result <- t(result)

# Get adjacency list
adj.sponsor <- get.adjacency(sponsor.network)

# Discard lower triangle
adj.sponsor[lower.tri(adj.sponsor)] <- 0
adj.sponsor[1:6, 1:6]

# Find strong collaborators
min(sort(adj.sponsor, decreasing = T)[1:10])
max.indices <- which(adj.sponsor >= min(sort(adj.sponsor, decreasing = T)[1:10]), arr.in = T)
export.names <- matrix(NA, ncol = 2, nrow = 10)
for(i in 1:nrow(max.indices)){
    export.names[i, 1] <- rownames(adj.sponsor)[max.indices[i,1]]
    export.names[i, 2] <- colnames(adj.sponsor)[max.indices[i,2]]
}

# Simplify network
E(sponsor.network)$weight <- 1
sponsor.network.weighted <- simplify(sponsor.network)
head(E(sponsor.network.weighted)$weight)

# Thin and plot network
plot.sponsor <- sponsor.network.weighted
plot.sponsor <- delete.edges(plot.sponsor, which(E(plot.sponsor)$weight < (mean(E(plot.sponsor)$weight) + sd(E(plot.sponsor)$weight))))
plot(plot.sponsor)












