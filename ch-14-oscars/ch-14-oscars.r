### --------------------------------------------------------------
### AUTOMATED DATA COLLECTION WITH R
### SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
###
### CODE CHAPTER 14: PREDICTING THE 2014 ACADEMY AWARDS USING TWITTER
### --------------------------------------------------------------

# load packages
library(streamR)
library(lubridate)
library(stringr)
library(plyr)

### 14.1 Twitter APIs: Overview
### --------------------------------------------------------------

# Filter the Twitter stream
filterStream("tweets_oscars.json", track = c("Oscars", "Oscars2014"),
timeout = 10800, oauth = twitCred)

# Parse tweets
tweets <- parseTweets("tweets_oscars.json", simplify = TRUE)

### 14.2 Twitter-based Forecast of the 2014 Academy Awards
### --------------------------------------------------------------

# Set locale of machine
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Get info on first tweet
dat$created_at[1]

# Parse values
dat$time <- as.POSIXct(dat$created_at, tz = "UTC", format = "%a %b %d %H
:%M:%S %z %Y")

# Round to nearest hour
dat$round_hour <- round_date(dat$time, unit = "hour")

# Plot the data
plot_time <- as.data.frame(table(dat$round_hour))
plot_time <- plot_time[-nrow(plot_time),]
plot(plot_time[,2], type = "l", xaxt = "n", xlab = "Hour", ylab = "Frequency")
axis(1, at = c(1, 20, 40, 60), labels = plot_time[c(1, 20, 40, 60), 1])

# Inspect one entry
unlist(dat[1234,])

# Search terms
actor <- c("matthew mcconaughey","christian bale","bruce dern","leonardo dicaprio","chiwetel ejiofor")

actress <- c("cate blanchett","amy adams","sandra bullock","judi dench","meryl streep"
)
film <- c("(12|twelve) years a slave","american hustle","captain phillips","dallas buyers club","gravity","nebraska","philomena","(the )?wolf of wall street"
)

# Detecting search terms
tmp_actor <- lapply(dat$lotext, str_detect, actor)dat_actor <- ldply(tmp_actor)colnames(dat_actor) <- c("mcconaughey", "bale", "dern", "dicaprio", "ejiofor")tmp_actress <- lapply(dat$lotext, str_detect, actress)dat_actress <- ldply(tmp_actress)colnames(dat_actress) <- c("blanchett", "adams", "bullock", "dench", "streep")tmp_film <- lapply(dat$lotext, str_detect, film)dat_film <- ldply(tmp_film)colnames(dat_film) <- c("twelve_years", "american_hustle", "capt_phillips", "dallas_buyers", "gravity", "nebraska", "philomena", "wolf_wallstreet")

# Get summary statistics
apply(dat_actor, 2, sum)
apply(dat_actress, 2, sum)
apply(dat_film, 2, sum)

# Approximate matching
tmp_actor2 <- lapply(actor, agrep, dat$lotext)length_actor <- unlist(lapply(tmp_actor2, length))names(length_actor) <- c("mcconaughey", "bale", "dern", "dicaprio", "ejiofor")tmp_actress2 <- lapply(actress, agrep, dat$lotext)length_actress <- unlist(lapply(tmp_actress2, length))names(length_actress) <- c("blanchett", "adams", "bullock", "dench", "streep")
length_actor
length_actress





