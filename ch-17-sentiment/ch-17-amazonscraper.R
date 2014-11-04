### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 17: Analyzing Sentiments of Product Reviews
### --------------------------------------------------------------

# Loading packages
# ------------------------------------------------------------------------- # 

library(tm)
library(textcat)
library(RTextTools)
library(vioplot)

### ------------------------------
### 17.3    Analyzing the Data ###
### ------------------------------

# connecting to DB
# ------------------------------------------------------------------------- # 

sqlite <- dbDriver("SQLite")
con <- dbConnect(sqlite, "amazonProductInfo.db")
dbListTables(con)

# read virtual table
allData <- dbReadTable(con, "AllData")
names(allData)
dim(allData)

### ------------------------------
### 17.3.1   Data preparation ###
### ------------------------------

# classify language
# ------------------------------------------------------------------------- # 

allData$language <- textcat(allData$reviewtext)
dim(allData)
allData <- allData[allData$language == "english",]
allData <- allData[!is.na(allData$reviewtext),]
dim(allData)

# create corpus
reviews <- Corpus(VectorSource(allData$reviewtext))

# preparation
reviews <- tm_map(reviews, removeNumbers)
reviews <- tm_map(reviews, str_replace_all, pattern = "[[:punct:]]", replacement = " ")
reviews <- tm_map(reviews, removeWords, words = stopwords("en"))
reviews <- tm_map(reviews, tolower)
reviews <- tm_map(reviews, stemDocument, language = "english")
reviews

### ------------------------------
### 17.3.2   Dictionary-based sentiment analysis ###
### ------------------------------

# load dictionary
pos <- readLines("opinion-lexicon-English/positive-words.txt")
pos <- pos[!str_detect(pos, "^;")]
pos <- pos[2:length(pos)]
neg <- readLines("opinion-lexicon-English/negative-words.txt")
neg <- neg[!str_detect(neg, "^;")]
neg <- neg[2:length(neg)]

# stem words
pos <- stemDocument(pos, language = "english")
pos <- pos[!duplicated(pos)]
neg <- stemDocument(neg, language = "english")
neg <- neg[!duplicated(neg)]

# sample sentiment terms
set.seed(123)
sample(pos, 10)
sample(neg, 10)

# generate Document-Term-Matrix
tdm.reviews.bin <- TermDocumentMatrix(reviews, control = list(weighting = weightBin))
tdm.reviews.bin <- removeSparseTerms(tdm.reviews.bin, 1-(5/length(reviews)))
tdm.reviews.bin

# score texts
pos.mat <- tdm.reviews.bin[rownames(tdm.reviews.bin) %in% pos,]
neg.mat <- tdm.reviews.bin[rownames(tdm.reviews.bin) %in% neg,]
pos.out <- apply(pos.mat, 2, sum)
neg.out <- apply(neg.mat, 2, sum)
senti.diff <- pos.out - neg.out
senti.diff[senti.diff == 0] <- NA

# inspect results
summary(senti.diff)
range(nchar(allData$reviewtext))

# set up plotting data
plot.dat <- data.frame(
    sentiment = senti.diff/nchar(allData$reviewtext),
    stars = allData$reviewstars)
plot.dat <- plot.dat[!is.na(plot.dat$sentiment),]

# plot data
vioplot(
    plot.dat$sentiment[plot.dat$stars == 1],
    plot.dat$sentiment[plot.dat$stars == 2],
    plot.dat$sentiment[plot.dat$stars == 3],
    plot.dat$sentiment[plot.dat$stars == 4],
    plot.dat$sentiment[plot.dat$stars == 5],
    horizontal = T,
    col = "grey"
)
axis(2, at = 3, labels = "Stars in review", line = 1, tick = F)
axis(1, at = 0.01, labels = "Estimated sentiment by number of characters", line = 1, tick = F)

# estimation based on headline
# Set up the corpus of titles
titles <- Corpus(VectorSource(allData$reviewtitle))
titles
# Perform data preparation
titles <- tm_map(titles, removeNumbers)
titles <- tm_map(titles, str_replace_all, pattern = "[[:punct:]]", replacement = " ")
titles <- tm_map(titles, removeWords, words = stopwords("en"))
titles <- tm_map(titles, tolower)
titles <- tm_map(titles, stemDocument, language = "english")
# Set up term-document matrix
tdm.titles <- TermDocumentMatrix(titles)
tdm.titles <- removeSparseTerms(tdm.titles, 1-(5/length(titles)))
tdm.titles
# Calculate the sentiment
pos.mat.tit <- tdm.titles[rownames(tdm.titles) %in% pos,]
neg.mat.tit <- tdm.titles[rownames(tdm.titles) %in% neg,]
pos.out.tit <- apply(pos.mat.tit, 2, sum)
neg.out.tit <- apply(neg.mat.tit, 2, sum)
senti.diff.tit <- pos.out.tit - neg.out.tit
senti.diff.tit[senti.diff.tit == 0] <- NA

# plot results
plot(jitter(senti.diff.tit), jitter(allData$reviewstars),
     col = rgb(0, 0, 0, 0.4),
     ylab = "Stars in review",
     xlab = "Estimated sentiment"
)
abline(v = 0, lty = 3)

### ------------------------------
### 17.3.3   Mining the content of reviews ###
### ------------------------------

# set up document-term-matrix
dtm.reviews <- DocumentTermMatrix(reviews)
dtm.reviews <- removeSparseTerms(dtm.reviews, 1-(5/length(reviews)))
N <- length(reviews)
container <- create_container(
    dtm.reviews,
    labels = allData$reviewsstars,
    trainSize = 1:2000,
    testSize = 2001:N,
    virgin = F
)
dtm.reviews

# train classifiers
maxent.model <- train_model(container, "MAXENT")
svm.model <- train_model(container, "SVM")

maxent.out <- classify_model(container, maxent.model)
svm.out <- classify_model(container, svm.model)

# create data frame with correct labels
labels.out <- data.frame(
    correct.label = as.numeric(allData$reviewstars[2001:N]),
    maxent = as.numeric(maxent.out[,1]),
    svm = as.numeric(svm.out[,1])
)

# plot results
plot(jitter(labels.out[,2]), jitter(labels.out[,1]),
     xlab = "Estimated stars",
     ylab = "True stars"
)

plot(jitter(labels.out[,3]), jitter(labels.out[,1]),
     xlab = "Estimated stars",
     ylab = "True stars"
)