library(stringr)
library(reshape2)

# Clean up
rm(list=ls())
cat("\04")

# Load data
train <- read.csv('data/train.csv');
test <- read.csv('data/test.csv');
words <- read.csv('data/words.csv');
users <- read.csv('data/users.csv');


# Clean up users dataset --------------------------------------------------
# first convert all empty cells with NA
users[users==""] <- NA

# clean 'working' column, encodes categories into integers
users$WORKING[users$WORKING == 'Employed 30+ hours a week'] <- 0;
users$WORKING[users$WORKING == 'Employed 8-29 hours per week'] <- 1;
users$WORKING[users$WORKING == 'Employed part-time less than 8 hours per week'] <- 2;
users$WORKING[users$WORKING == 'Full-time housewife / househusband'] <- 3;
users$WORKING[users$WORKING == 'Full-time student'] <- 4;
users$WORKING[users$WORKING == 'In unpaid employment (e.g. voluntary work)'] <- 5;
users$WORKING[users$WORKING == 'Other'] <- 6;
users$WORKING[users$WORKING == 'Part-time student'] <- 7;
users$WORKING[users$WORKING == 'Prefer not to state'] <- 8;
users$WORKING[users$WORKING == 'Retired from full-time employment (30+ hours per week)'] <- 9;
users$WORKING[users$WORKING == 'Retired from self-employment'] <- 10;
users$WORKING[users$WORKING == 'Self-employed'] <- 11;
users$WORKING[users$WORKING == 'Temporarily unemployed'] <- 12;
users$WORKING[is.na(users$WORKING)] <- 13   # convert missing values in WORKING to a new category
users$WORKING <- as.character(users$WORKING)

# clean 'music' column, encode categories into integers
users$MUSIC[users$MUSIC == 'Music has no particular interest for me'] <- 0;
users$MUSIC[users$MUSIC == 'Music is no longer as important as it used to be to me'] <- 1;
users$MUSIC[users$MUSIC == 'I like music but it does not feature heavily in my life'] <- 2;
users$MUSIC[users$MUSIC == 'Music is important to me but not necessarily more important than other hobbies or interests'] <- 3;
users$MUSIC[users$MUSIC == 'Music is important to me but not necessarily more important'] <- 3;
users$MUSIC[users$MUSIC == 'Music means a lot to me and is a passion of mine'] <- 4;
users$MUSIC <- as.integer(users$MUSIC)

# clean 'list_own' and 'list_back', drop text and only leave with integers, drop rows that contains 'more than 16 hours/16+hours'
users$LIST_OWN[users$LIST_OWN == 'Less than an hour'] <- 0
users$LIST_OWN[users$LIST_OWN == '16+ hours'] <- 16
users$LIST_OWN[users$LIST_OWN == 'More than 16 hours'] <- 16
temp <-colsplit(users$LIST_OWN, " ", c("LIST_OWN", "delete"))
users$LIST_OWN <- temp[,1]
users$LIST_OWN <- as.integer(users$LIST_OWN)

users$LIST_BACK[users$LIST_BACK == 'Less than an hour'] <- 0;
users$LIST_BACK[users$LIST_BACK == '16+ hours'] <- 16
users$LIST_BACK[users$LIST_BACK == 'More than 16 hours'] <- 16
temp <-colsplit(users$LIST_BACK, " ", c("LIST_BACK", "delete"))
users$LIST_OWN <- temp[,1]
users$LIST_BACK <- as.integer(users$LIST_BACK)

# clean gender 
users$GENDER[users$GENDER == 'Male'] <- 1;
users$GENDER[users$GENDER == 'Female'] <- 0;
users$GENDER <- as.integer(users$GENDER)

# clean region
users$REGION[users$REGION == 'North'] <- 1;
users$REGION[users$REGION == 'East'] <- 2;
users$REGION[users$REGION == 'West'] <- 3;
users$REGION[users$REGION == 'South'] <- 4;
users$REGION[users$REGION == 'Midlands'] <- 5;
users$REGION[users$REGION == 'Centre'] <- 6;
users$REGION[users$REGION == 'Northern Ireland'] <- 7;
users$REGION[users$REGION == 'North Ireland'] <- 7;
users$REGION <- as.character(users$REGION)


# Clean Up 'words' dataset ------------------------------------------------

# Clean 'Heard_of' column by encoding categories into numbers
words$HEARD_OF[words$HEARD_OF == 'Never heard of'] <- 0;
words$HEARD_OF[words$HEARD_OF == 'Ever heard of'] <- 1;
words$HEARD_OF[words$HEARD_OF == 'Ever heard music by'] <- 1;
words$HEARD_OF[words$HEARD_OF == 'Heard of'] <- 2;
words$HEARD_OF[words$HEARD_OF == 'Heard of and listened to music EVER'] <- 3;
words$HEARD_OF[words$HEARD_OF == 'Heard of and listened to music RECENTLY'] <- 4;
words$HEARD_OF[words$HEARD_OF == 'Listened to recently'] <- 5;
words$HEARD_OF <- as.integer(words$HEARD_OF)

# Clean 'Own_artist_music' column by encoding categories into numbers
words$OWN_ARTIST_MUSIC[words$OWN_ARTIST_MUSIC == 'don`t know'] <- 0;
words$OWN_ARTIST_MUSIC[str_detect(words$OWN_ARTIST_MUSIC,'know')] <- 0;
words$OWN_ARTIST_MUSIC[words$OWN_ARTIST_MUSIC == 'Own none of their music'] <- 1;
words$OWN_ARTIST_MUSIC[words$OWN_ARTIST_MUSIC == 'Own a little of their music'] <- 2;
words$OWN_ARTIST_MUSIC[words$OWN_ARTIST_MUSIC == 'Own a lot of their music'] <- 3;
words$OWN_ARTIST_MUSIC[words$OWN_ARTIST_MUSIC == 'Own all or most of their music'] <- 4;
words$OWN_ARTIST_MUSIC <- as.integer(words$OWN_ARTIST_MUSIC)

# Drop last column
words <- words[,1:length(words)-1]


# Merge Data sets ---------------------------------------------------------
# Merge users and words
users_words <- merge(users, words, by.x = "RESPID", by.y =  "User")
# Import training data
train <- read.csv('data/train.csv')
# Join training data with users and artisits information 
train_all <- merge(train, users_words, by.x = c("User", "Artist"), by.y = c("RESPID", "Artist"))


# Deal with missing values ------------------------------------------------
# examine the count and percentage of missing values for each column
sort(colMeans(is.na(train_all)), decreasing = TRUE)
# convert missing values in Question columns into their averages
train_all$Q18[is.na(train_all$Q18)] <- as.integer(round(mean(train_all$Q18[!is.na(train_all$Q18)])))
train_all$Q19[is.na(train_all$Q19)] <- as.integer(round(mean(train_all$Q19[!is.na(train_all$Q19)])))
train_all$Q16[is.na(train_all$Q16)] <- as.integer(round(mean(train_all$Q16[!is.na(train_all$Q16)])))
# convert missing values in LIST columns into their averages
train_all$LIST_BACK[is.na(train_all$LIST_BACK)] <- as.integer(round(mean(train_all$LIST_BACK[!is.na(train_all$LIST_BACK)])))
train_all$LIST_OWN[is.na(train_all$LIST_OWN)] <- as.integer(round(mean(train_all$LIST_OWN[!is.na(train_all$LIST_OWN)])))
# convert missing values in REGION columns into the most popular region
table(train_all$REGION) # seems like North is the most popular region, its corresponding encoding is 1
train_all$REGION[is.na(train_all$REGION)] <- 1
# convert missing values in AGE columns into the average age
train_all$AGE[is.na(train_all$AGE)] <- as.integer(round(mean(train_all$AGE[!is.na(train_all$AGE)])))

# convert missing values in HEARD_OF column into the most popular category
table(train_all$HEARD_OF)
max(table(train_all$HEARD_OF)) # seems like 'Never Heard Of' is the most popular category
train_all$HEARD_OF[is.na(train_all$HEARD_OF)] <- 0
# convert missing values in OWN_ARTIST_MUSIC column into 0, which assume the user doesn't know the artist
train_all$OWN_ARTIST_MUSIC[is.na(train_all$OWN_ARTIST_MUSIC)]<-0
# convert missing values in LIKE_ARTIST column into its average 
train_all$LIKE_ARTIST[is.na(train_all$LIKE_ARTIST)] <- as.integer(round(mean(train_all$LIKE_ARTIST[!is.na(train_all$LIKE_ARTIST)])))
# get rid of columns with over 90% missing value
train_all[,c("Wholesome", "Legendary", "Old", "Pioneer", "Dark", "Worldly", "Nostalgic", "Progressive", "Iconic")] <- list(NULL)

# The rest are just descriptive words
sort(colMeans(is.na(train_all)), decreasing = TRUE)
# convert all other missing values for the description words into 0, assuming the user doesn't select them
train_all[is.na(train_all)] <- 0

# Fix data type and examine the Cleaning Result ---------------------------------------------
# Fix data type
train_all$GENDER <- as.character(train_all$GENDER)
# Re=organize column order
train_all <- train_all[,c(1:12, 32:34, 13:31, 35:107)]
train_all_int <- c(13:14, 16:ncol(train_all))
train_all[train_all_int] <- sapply(train_all[train_all_int], as.integer)

# Examine the cleaning result
str(train_all)
sort(colMeans(is.na(train_all)), decreasing = TRUE)


# Output Cleaned CSV ------------------------------------------------------
write.csv(users, 'train_all.csv', row.names=FALSE);



