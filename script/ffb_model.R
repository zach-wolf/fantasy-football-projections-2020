library(OneR)
library(caret)
library(dplyr)
library(ggplot2)

# Set working directory and load in the dataset
setwd("~/Documents/FFB Project")
dat2019 <- read.csv("fantasy-stats-receiving-2019.csv", header =  T)
dat2018 <- read.csv("fantasy-stats-receiving-2018.csv", header =  T)
dat2017 <- read.csv("fantasy-stats-receiving-2017.csv", header =  T)
dat2016 <- read.csv("fantasy-stats-receiving-2016.csv", header =  T)
dat2015 <- read.csv("fantasy-stats-receiving-2015.csv", header =  T)
dat2014 <- read.csv("fantasy-stats-receiving-2014.csv", header =  T)
dat2013 <- read.csv("fantasy-stats-receiving-2013.csv", header =  T)
dat2012 <- read.csv("fantasy-stats-receiving-2012.csv", header =  T)
dat2011 <- read.csv("fantasy-stats-receiving-2011.csv", header =  T)
dat2010 <- read.csv("fantasy-stats-receiving-2010.csv", header =  T)

#### DATA ####
## 2019 DATASET
# Remove blank columns that you have to pay for
dat2019 <- dat2019[colSums(!is.na(dat2019)) > 0]

# Only include players who recorded fantasy points
dat2019 <- dat2019[dat2019$fantasyPts > 0,]

# Only include players with at least 8 games (half season)
dat2019 <- dat2019[dat2019$games >= 4, ]

# Remove OL (most probably got removed in last step, but just in case)
dat2019 <- dat2019[dat2019$position %in% c('RB','WR','TE'),]

# Create points per game variable
dat2019$ptsPerGame <- dat2019$fantasyPts/dat2019$games

# Add Season column
dat2019$Season <- 2019

dat2019$NextSeasonPts <- NA
dat2019$NextSeasonPtsPerGame <- NA


## 2018 DATASET 
# Remove blank columns that you have to pay for
dat2018 <- dat2018[colSums(!is.na(dat2018)) > 0]

# Only include players who recorded fantasy points
dat2018 <- dat2018[dat2018$fantasyPts > 0,]

# Only include players with at least 8 games (half season)
dat2018 <- dat2018[dat2018$games >= 4, ]

# Create points per game variable
dat2018$ptsPerGame <- dat2018$fantasyPts/dat2018$games

# Remove OL (most probably got removed in last step, but just in case)
dat2018 <- dat2018[dat2018$position %in% c('RB','WR','TE'),]

# Add Season column
dat2018$Season <- 2018

dat2018$NextSeasonPts <- 0
for(i in 1:nrow(dat2018)){
  dat2018$NextSeasonPts[i] <- ifelse(length(dat2019[which(as.character(dat2019$player)==as.character(dat2018$player[i])),"fantasyPts"])>0,
                                     dat2019[which(as.character(dat2019$player)==as.character(dat2018$player[i])),"fantasyPts"],
                                     0)
}

dat2018$NextSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2018)){
  dat2018$NextSeasonPtsPerGame[i] <- ifelse(length(dat2019[which(as.character(dat2019$player)==as.character(dat2018$player[i])),"ptsPerGame"])>0,
                                     dat2019[which(as.character(dat2019$player)==as.character(dat2018$player[i])),"ptsPerGame"],
                                     0)
}

dat2019$LastSeasonPts <- 0
for(i in 1:nrow(dat2019)){
  dat2019$LastSeasonPts[i] <- ifelse(length(dat2018[which(as.character(dat2018$player)==as.character(dat2019$player[i])),"fantasyPts"])>0,
                                     dat2018[which(as.character(dat2018$player)==as.character(dat2019$player[i])),"fantasyPts"],
                                     NA)
}

dat2019$LastSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2019)){
  dat2019$LastSeasonPtsPerGame[i] <- ifelse(length(dat2018[which(as.character(dat2018$player)==as.character(dat2019$player[i])),"ptsPerGame"])>0,
                                            dat2018[which(as.character(dat2018$player)==as.character(dat2019$player[i])),"ptsPerGame"],
                                            NA)
}

# Only keep players who scored points the next year
dat2018 <- dat2018[dat2018$NextSeasonPts > 0,]


## 2017 DATASET 
# Remove blank columns that you have to pay for
dat2017 <- dat2017[colSums(!is.na(dat2017)) > 0]

# Only include players who recorded fantasy points
dat2017 <- dat2017[dat2017$fantasyPts > 0,]

# Only include players with at least 8 games (half season)
dat2017 <- dat2017[dat2017$games >= 4, ]

# Create points per game variable
dat2017$ptsPerGame <- dat2017$fantasyPts/dat2017$games

# Remove OL (most probably got removed in last step, but just in case)
dat2017 <- dat2017[dat2017$position %in% c('RB','WR','TE'),]

# Add Season column
dat2017$Season <- 2017

dat2017$NextSeasonPts <- 0
for(i in 1:nrow(dat2017)){
  dat2017$NextSeasonPts[i] <- ifelse(length(dat2018[which(as.character(dat2018$player)==as.character(dat2017$player[i])),"fantasyPts"])>0,
                                     dat2018[which(as.character(dat2018$player)==as.character(dat2017$player[i])),"fantasyPts"],
                                     0)
}

dat2017$NextSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2017)){
  dat2017$NextSeasonPtsPerGame[i] <- ifelse(length(dat2018[which(as.character(dat2018$player)==as.character(dat2017$player[i])),"ptsPerGame"])>0,
                                            dat2018[which(as.character(dat2018$player)==as.character(dat2017$player[i])),"ptsPerGame"],
                                            0)
}

dat2018$LastSeasonPts <- 0
for(i in 1:nrow(dat2018)){
  dat2018$LastSeasonPts[i] <- ifelse(length(dat2017[which(as.character(dat2017$player)==as.character(dat2018$player[i])),"fantasyPts"])>0,
                                     dat2017[which(as.character(dat2017$player)==as.character(dat2018$player[i])),"fantasyPts"],
                                     NA)
}

dat2018$LastSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2018)){
  dat2018$LastSeasonPtsPerGame[i] <- ifelse(length(dat2017[which(as.character(dat2017$player)==as.character(dat2018$player[i])),"ptsPerGame"])>0,
                                            dat2017[which(as.character(dat2017$player)==as.character(dat2018$player[i])),"ptsPerGame"],
                                            NA)
}

# Only keep players who scored points the next year
dat2017 <- dat2017[dat2017$NextSeasonPts > 0,]


## 2016 DATASET
# Remove blank columns that you have to pay for
dat2016 <- dat2016[colSums(!is.na(dat2016)) > 0]

# Only include players who recorded fantasy points
dat2016 <- dat2016[dat2016$fantasyPts > 0,]

# Only include players with at least 8 games (half season)
dat2016 <- dat2016[dat2016$games >= 4, ]

# Create points per game variable
dat2016$ptsPerGame <- dat2016$fantasyPts/dat2016$games

# Remove OL (most probably got removed in last step, but just in case)
dat2016 <- dat2016[dat2016$position %in% c('RB','WR','TE'),]

# Add Season column
dat2016$Season <- 2016

dat2016$NextSeasonPts <- 0
for(i in 1:nrow(dat2016)){
  dat2016$NextSeasonPts[i] <- ifelse(length(dat2017[which(as.character(dat2017$player)==as.character(dat2016$player[i])),"fantasyPts"])>0,
                                     dat2017[which(as.character(dat2017$player)==as.character(dat2016$player[i])),"fantasyPts"],
                                     0)
}

dat2016$NextSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2016)){
  dat2016$NextSeasonPtsPerGame[i] <- ifelse(length(dat2017[which(as.character(dat2017$player)==as.character(dat2016$player[i])),"ptsPerGame"])>0,
                                            dat2017[which(as.character(dat2017$player)==as.character(dat2016$player[i])),"ptsPerGame"],
                                            0)
}

dat2017$LastSeasonPts <- 0
for(i in 1:nrow(dat2017)){
  dat2017$LastSeasonPts[i] <- ifelse(length(dat2016[which(as.character(dat2016$player)==as.character(dat2017$player[i])),"fantasyPts"])>0,
                                     dat2016[which(as.character(dat2016$player)==as.character(dat2017$player[i])),"fantasyPts"],
                                     NA)
}

dat2017$LastSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2017)){
  dat2017$LastSeasonPtsPerGame[i] <- ifelse(length(dat2016[which(as.character(dat2016$player)==as.character(dat2017$player[i])),"ptsPerGame"])>0,
                                            dat2016[which(as.character(dat2016$player)==as.character(dat2017$player[i])),"ptsPerGame"],
                                            NA)
}

# Only keep players who scored points the next year
dat2016 <- dat2016[dat2016$NextSeasonPts > 0,]


## 2015 DATASET 
# Remove blank columns that you have to pay for
dat2015 <- dat2015[colSums(!is.na(dat2015)) > 0]

# Only include players who recorded fantasy points
dat2015 <- dat2015[dat2015$fantasyPts > 0,]

# Only include players with at least 8 games (half season)
dat2015 <- dat2015[dat2015$games >= 4, ]

# Create points per game variable
dat2015$ptsPerGame <- dat2015$fantasyPts/dat2015$games

# Remove OL (most probably got removed in last step, but just in case)
dat2015 <- dat2015[dat2015$position %in% c('RB','WR','TE'),]

# Add Season column
dat2015$Season <- 2015

dat2015$NextSeasonPts <- 0
for(i in 1:nrow(dat2015)){
  dat2015$NextSeasonPts[i] <- ifelse(length(dat2016[which(as.character(dat2016$player)==as.character(dat2015$player[i])),"fantasyPts"])>0,
                                     dat2016[which(as.character(dat2016$player)==as.character(dat2015$player[i])),"fantasyPts"],
                                     0)
}

dat2015$NextSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2015)){
  dat2015$NextSeasonPtsPerGame[i] <- ifelse(length(dat2016[which(as.character(dat2016$player)==as.character(dat2015$player[i])),"ptsPerGame"])>0,
                                            dat2016[which(as.character(dat2016$player)==as.character(dat2015$player[i])),"ptsPerGame"],
                                            0)
}

dat2016$LastSeasonPts <- 0
for(i in 1:nrow(dat2016)){
  dat2016$LastSeasonPts[i] <- ifelse(length(dat2015[which(as.character(dat2015$player)==as.character(dat2016$player[i])),"fantasyPts"])>0,
                                     dat2015[which(as.character(dat2015$player)==as.character(dat2016$player[i])),"fantasyPts"],
                                     NA)
}

dat2016$LastSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2016)){
  dat2016$LastSeasonPtsPerGame[i] <- ifelse(length(dat2015[which(as.character(dat2015$player)==as.character(dat2016$player[i])),"ptsPerGame"])>0,
                                            dat2015[which(as.character(dat2015$player)==as.character(dat2016$player[i])),"ptsPerGame"],
                                            NA)
}

# Only keep players who scored points the next year
dat2015 <- dat2015[dat2015$NextSeasonPts > 0,]


## 2014 DATASET 
# Remove blank columns that you have to pay for
dat2014 <- dat2014[colSums(!is.na(dat2014)) > 0]

# Only include players who recorded fantasy points
dat2014 <- dat2014[dat2014$fantasyPts > 0,]

# Only include players with at least 8 games (half season)
dat2014 <- dat2014[dat2014$games >= 4, ]

# Create points per game variable
dat2014$ptsPerGame <- dat2014$fantasyPts/dat2014$games

# Remove OL (most probably got removed in last step, but just in case)
dat2014 <- dat2014[dat2014$position %in% c('RB','WR','TE'),]

# Add Season column
dat2014$Season <- 2014

dat2014$NextSeasonPts <- 0
for(i in 1:nrow(dat2014)){
  dat2014$NextSeasonPts[i] <- ifelse(length(dat2015[which(as.character(dat2015$player)==as.character(dat2014$player[i])),"fantasyPts"])>0,
                                     dat2015[which(as.character(dat2015$player)==as.character(dat2014$player[i])),"fantasyPts"],
                                     0)
}

dat2014$NextSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2014)){
  dat2014$NextSeasonPtsPerGame[i] <- ifelse(length(dat2015[which(as.character(dat2015$player)==as.character(dat2014$player[i])),"ptsPerGame"])>0,
                                            dat2015[which(as.character(dat2015$player)==as.character(dat2014$player[i])),"ptsPerGame"],
                                            0)
}

dat2015$LastSeasonPts <- 0
for(i in 1:nrow(dat2015)){
  dat2015$LastSeasonPts[i] <- ifelse(length(dat2014[which(as.character(dat2014$player)==as.character(dat2015$player[i])),"fantasyPts"])>0,
                                     dat2014[which(as.character(dat2014$player)==as.character(dat2015$player[i])),"fantasyPts"],
                                     NA)
}

dat2015$LastSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2015)){
  dat2015$LastSeasonPtsPerGame[i] <- ifelse(length(dat2014[which(as.character(dat2014$player)==as.character(dat2015$player[i])),"ptsPerGame"])>0,
                                            dat2014[which(as.character(dat2014$player)==as.character(dat2015$player[i])),"ptsPerGame"],
                                            NA)
}

# Only keep players who scored points the next year
dat2014 <- dat2014[dat2014$NextSeasonPts > 0,]


## 2013 DATASET 
# Remove blank columns that you have to pay for
dat2013 <- dat2013[colSums(!is.na(dat2013)) > 0]

# Only include players who recorded fantasy points
dat2013 <- dat2013[dat2013$fantasyPts > 0,]

# Only include players with at least 8 games (half season)
dat2013 <- dat2013[dat2013$games >= 4, ]

# Create points per game variable
dat2013$ptsPerGame <- dat2013$fantasyPts/dat2013$games

# Remove OL (most probably got removed in last step, but just in case)
dat2013 <- dat2013[dat2013$position %in% c('RB','WR','TE'),]

# Add Season column
dat2013$Season <- 2013

dat2013$NextSeasonPts <- 0
for(i in 1:nrow(dat2013)){
  dat2013$NextSeasonPts[i] <- ifelse(length(dat2014[which(as.character(dat2014$player)==as.character(dat2013$player[i])),"fantasyPts"])>0,
                                     dat2014[which(as.character(dat2014$player)==as.character(dat2013$player[i])),"fantasyPts"],
                                     0)
}

dat2013$NextSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2013)){
  dat2013$NextSeasonPtsPerGame[i] <- ifelse(length(dat2014[which(as.character(dat2014$player)==as.character(dat2013$player[i])),"ptsPerGame"])>0,
                                            dat2014[which(as.character(dat2014$player)==as.character(dat2013$player[i])),"ptsPerGame"],
                                            0)
}

dat2014$LastSeasonPts <- 0
for(i in 1:nrow(dat2014)){
  dat2014$LastSeasonPts[i] <- ifelse(length(dat2013[which(as.character(dat2013$player)==as.character(dat2014$player[i])),"fantasyPts"])>0,
                                     dat2013[which(as.character(dat2013$player)==as.character(dat2014$player[i])),"fantasyPts"],
                                     NA)
}

dat2014$LastSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2014)){
  dat2014$LastSeasonPtsPerGame[i] <- ifelse(length(dat2013[which(as.character(dat2013$player)==as.character(dat2014$player[i])),"ptsPerGame"])>0,
                                            dat2013[which(as.character(dat2013$player)==as.character(dat2014$player[i])),"ptsPerGame"],
                                            NA)
}

# Only keep players who scored points the next year
dat2013 <- dat2013[dat2013$NextSeasonPts > 0,]


## 2012 DATASET
# Remove blank columns that you have to pay for
dat2012 <- dat2012[colSums(!is.na(dat2012)) > 0]

# Only include players who recorded fantasy points
dat2012 <- dat2012[dat2012$fantasyPts > 0,]

# Only include players with at least 8 games (half season)
dat2012 <- dat2012[dat2012$games >= 4, ]

# Create points per game variable
dat2012$ptsPerGame <- dat2012$fantasyPts/dat2012$games

# Remove OL (most probably got removed in last step, but just in case)
dat2012 <- dat2012[dat2012$position %in% c('RB','WR','TE'),]

# Add Season column
dat2012$Season <- 2012

dat2012$NextSeasonPts <- 0
for(i in 1:nrow(dat2012)){
  dat2012$NextSeasonPts[i] <- ifelse(length(dat2013[which(as.character(dat2013$player)==as.character(dat2012$player[i])),"fantasyPts"])>0,
                                     dat2013[which(as.character(dat2013$player)==as.character(dat2012$player[i])),"fantasyPts"],
                                     0)
}

dat2012$NextSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2012)){
  dat2012$NextSeasonPtsPerGame[i] <- ifelse(length(dat2013[which(as.character(dat2013$player)==as.character(dat2012$player[i])),"ptsPerGame"])>0,
                                            dat2013[which(as.character(dat2013$player)==as.character(dat2012$player[i])),"ptsPerGame"],
                                            0)
}

dat2013$LastSeasonPts <- 0
for(i in 1:nrow(dat2013)){
  dat2013$LastSeasonPts[i] <- ifelse(length(dat2012[which(as.character(dat2012$player)==as.character(dat2013$player[i])),"fantasyPts"])>0,
                                     dat2012[which(as.character(dat2012$player)==as.character(dat2013$player[i])),"fantasyPts"],
                                     NA)
}

dat2013$LastSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2013)){
  dat2013$LastSeasonPtsPerGame[i] <- ifelse(length(dat2012[which(as.character(dat2012$player)==as.character(dat2013$player[i])),"ptsPerGame"])>0,
                                            dat2012[which(as.character(dat2012$player)==as.character(dat2013$player[i])),"ptsPerGame"],
                                            NA)
}

# Only keep players who scored points the next year
dat2012 <- dat2012[dat2012$NextSeasonPts > 0,]


## 2011 DATASET
# Remove blank columns that you have to pay for
dat2011 <- dat2011[colSums(!is.na(dat2011)) > 0]

# Only include players who recorded fantasy points
dat2011 <- dat2011[dat2011$fantasyPts > 0,]

# Only include players with at least 8 games (half season)
dat2011 <- dat2011[dat2011$games >= 4, ]

# Create points per game variable
dat2011$ptsPerGame <- dat2011$fantasyPts/dat2011$games

# Remove OL (most probably got removed in last step, but just in case)
dat2011 <- dat2011[dat2011$position %in% c('RB','WR','TE'),]

# Add Season column
dat2011$Season <- 2011

dat2011$NextSeasonPts <- 0
for(i in 1:nrow(dat2011)){
  dat2011$NextSeasonPts[i] <- ifelse(length(dat2012[which(as.character(dat2012$player)==as.character(dat2011$player[i])),"fantasyPts"])>0,
                                     dat2012[which(as.character(dat2012$player)==as.character(dat2011$player[i])),"fantasyPts"],
                                     0)
}

dat2011$NextSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2011)){
  dat2011$NextSeasonPtsPerGame[i] <- ifelse(length(dat2012[which(as.character(dat2012$player)==as.character(dat2011$player[i])),"ptsPerGame"])>0,
                                            dat2012[which(as.character(dat2012$player)==as.character(dat2011$player[i])),"ptsPerGame"],
                                            0)
}

dat2012$LastSeasonPts <- 0
for(i in 1:nrow(dat2012)){
  dat2012$LastSeasonPts[i] <- ifelse(length(dat2011[which(as.character(dat2011$player)==as.character(dat2012$player[i])),"fantasyPts"])>0,
                                     dat2011[which(as.character(dat2011$player)==as.character(dat2012$player[i])),"fantasyPts"],
                                     NA)
}

dat2012$LastSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2012)){
  dat2012$LastSeasonPtsPerGame[i] <- ifelse(length(dat2011[which(as.character(dat2011$player)==as.character(dat2012$player[i])),"ptsPerGame"])>0,
                                            dat2011[which(as.character(dat2011$player)==as.character(dat2012$player[i])),"ptsPerGame"],
                                            NA)
}

# Only keep players who scored points the next year
dat2011 <- dat2011[dat2011$NextSeasonPts > 0,]


## 2010 DATASET
# Remove blank columns that you have to pay for
dat2010 <- dat2010[colSums(!is.na(dat2010)) > 0]

# Only include players who recorded fantasy points
dat2010 <- dat2010[dat2010$fantasyPts > 0,]

# Only include players with at least 8 games (half season)
dat2010 <- dat2010[dat2010$games >= 4, ]

# Create points per game variable
dat2010$ptsPerGame <- dat2010$fantasyPts/dat2010$games

# Remove OL (most probably got removed in last step, but just in case)
dat2010 <- dat2010[dat2010$position %in% c('RB','WR','TE'),]

# Add Season column
dat2010$Season <- 2010

dat2010$NextSeasonPts <- 0
for(i in 1:nrow(dat2010)){
  dat2010$NextSeasonPts[i] <- ifelse(length(dat2011[which(as.character(dat2011$player)==as.character(dat2010$player[i])),"fantasyPts"])>0,
                                     dat2011[which(as.character(dat2011$player)==as.character(dat2010$player[i])),"fantasyPts"],
                                     0)
}

dat2010$NextSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2010)){
  dat2010$NextSeasonPtsPerGame[i] <- ifelse(length(dat2011[which(as.character(dat2011$player)==as.character(dat2010$player[i])),"ptsPerGame"])>0,
                                            dat2011[which(as.character(dat2011$player)==as.character(dat2010$player[i])),"ptsPerGame"],
                                            0)
}

dat2011$LastSeasonPts <- 0
for(i in 1:nrow(dat2011)){
  dat2011$LastSeasonPts[i] <- ifelse(length(dat2010[which(as.character(dat2010$player)==as.character(dat2011$player[i])),"fantasyPts"])>0,
                                     dat2010[which(as.character(dat2010$player)==as.character(dat2011$player[i])),"fantasyPts"],
                                     NA)
}

dat2011$LastSeasonPtsPerGame <- 0
for(i in 1:nrow(dat2011)){
  dat2011$LastSeasonPtsPerGame[i] <- ifelse(length(dat2010[which(as.character(dat2010$player)==as.character(dat2011$player[i])),"ptsPerGame"])>0,
                                            dat2010[which(as.character(dat2010$player)==as.character(dat2011$player[i])),"ptsPerGame"],
                                            NA)
}

dat2010$LastSeasonPts <- NA
dat2010$LastSeasonPtsPerGame <- NA

# Only keep players who scored points the next year
dat2010 <- dat2010[dat2010$NextSeasonPts > 0,]


## Combine all years into one dataset
dat <- rbind(dat2013, dat2014, dat2015, dat2016, dat2017, dat2018, dat2019)

# Vita Vea shows as TE but is a Nose Tackle
dat <- dat[dat$player != "Vita Vea", ]

# Add touches feature to dataset
dat$Touches <- dat$recRec + dat$rushCarries

summary(dat)

# Impute missing last season numbers with mean
dat <- transform(dat, LastSeasonPts = ifelse(is.na(LastSeasonPts), mean(LastSeasonPts, na.rm=TRUE), LastSeasonPts))
dat <- transform(dat, LastSeasonPtsPerGame = ifelse(is.na(LastSeasonPtsPerGame), mean(LastSeasonPtsPerGame, na.rm=TRUE), LastSeasonPtsPerGame))


set.seed(69)
#### RB ####
# Subset running backs
datRB <- dat[dat$position=="RB",]

# Remove pat columns (these happen far too infrequently)
datRB <- datRB[, -which(names(datRB) %in% c("patConversions","patAttempts"))]

# Remove 2019 data to build model
datRBsmall <- datRB[datRB$Season!=2019,]

# Create index to split into training and test set
index <- createDataPartition(datRBsmall$NextSeasonPtsPerGame, p = 0.75, list = FALSE)

# Subset training set with index
dat.training <- datRBsmall[index,]

# Subset test set with index
dat.test <- datRBsmall[-index,]

## Look at correlation of features
# calculate correlation matrix
correlationMatrix <- cor(datRBsmall[, -which(names(dat.training) %in% c("player","team","position","Season","NextSeasonPts","NextSeasonPtsPerGame"))])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)

## Use Recursive Feature Elimination to select features for model
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(datRBsmall[, -which(names(dat.training) %in% c("player","team","position","Season","NextSeasonPts","NextSeasonPtsPerGame"))], datRBsmall[, "NextSeasonPtsPerGame"], sizes=c(1:32), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

train_control <- trainControl(method="boot", number=100)
rb_performance <- data.frame()

# Train a model with all variables to compare against others
model_rf1_rb <- train(dat.training[, -which(names(dat.training) %in% c("player","team","position","Season","NextSeasonPts","NextSeasonPtsPerGame"))],
                  dat.training[, "NextSeasonPtsPerGame"],
                  method = 'rf',
                  family = "response",
                  trControl = train_control,
                  preProcess = c('range'))

varImp(model_rf1_rb)

predictions <- predict(object=model_rf1_rb,
                       dat.test[, -which(names(dat.test) %in% c("player","team","position","Season","NextSeasonPts","NextSeasonPtsPerGame"))])

# Add to performance data table
data.frame(dat.test$player,predictions,dat.test$NextSeasonPtsPerGame)
actuals_preds <- data.frame(cbind(actuals=dat.test$NextSeasonPtsPerGame, predicteds=predictions))
(correlation_accuracy <- cor(actuals_preds)) 
rb_performance[1,1] <- "RF (all features)"
rb_performance[1,2] <- max(model_rf1_rb$results[,"Rsquared"])
rb_performance[1,3] <- min(model_rf1_rb$results[,"RMSE"])
rb_performance[1,4] <- correlation_accuracy[1,2]
rb_performance[1,5] <- correlation_accuracy[1,2]^2 
rb_performance[1,6] <- rmse(dat.test$NextSeasonPtsPerGame, predictions) 
colnames(rb_performance) <- c("Model", "R^2 Train", "RMSE Train", "Correlation Test", "R^2 Test", "RMSE Test")

# Train a model with selected rfe variables
model_rf_rb <- train(dat.training[, c(predictors(results))],
                  dat.training[, "NextSeasonPtsPerGame"],
                  method = 'rf',
                  family = "response",
                  trControl = train_control,
                  preProcess = c('range'))

varImp(model_rf_rb)

predictions <- predict(object=model_rf_rb,
                       dat.test[, c(predictors(results))])

# Add to performance data table
data.frame(dat.test$player,predictions,dat.test$NextSeasonPtsPerGame)
actuals_preds <- data.frame(cbind(actuals=dat.test$NextSeasonPtsPerGame, predicteds=predictions))
(correlation_accuracy <- cor(actuals_preds)) 
rb_performance[2,1] <- "RF"
rb_performance[2,2] <- max(model_rf_rb$results[,"Rsquared"])
rb_performance[2,3] <- min(model_rf_rb$results[,"RMSE"])
rb_performance[2,4] <- correlation_accuracy[1,2]
rb_performance[2,5] <- correlation_accuracy[1,2]^2 
rb_performance[2,6] <- rmse(dat.test$NextSeasonPtsPerGame, predictions) 

# lm model with rfe features
model_lm_rb <- train(dat.training[, c(predictors(results))],
                     dat.training[, "NextSeasonPtsPerGame"],
                     method = 'lm',
                     preProcess = c('range'))

varImp(model_lm_rb)

predictions <- predict(object=model_lm_rb,
                       dat.test[, c(predictors(results))])

# Add to performance data table
data.frame(dat.test$player,predictions,dat.test$NextSeasonPtsPerGame)
actuals_preds <- data.frame(cbind(actuals=dat.test$NextSeasonPtsPerGame, predicteds=predictions))
(correlation_accuracy <- cor(actuals_preds)) 
rb_performance[3,1] <- "Linear Regression"
rb_performance[3,2] <- max(model_lm_rb$results[,"Rsquared"])
rb_performance[3,3] <- min(model_lm_rb$results[,"RMSE"])
rb_performance[3,4] <- correlation_accuracy[1,2]
rb_performance[3,5] <- correlation_accuracy[1,2]^2 
rb_performance[3,6] <- rmse(dat.test$NextSeasonPtsPerGame, predictions) 

# knn model with rfe features
model_knn_rb <- train(dat.training[, c(predictors(results))],
                     dat.training[, "NextSeasonPtsPerGame"],
                     method = 'knn',
                     preProcess = c('range'))

varImp(model_knn_rb)

predictions <- predict(object=model_knn_rb,
                       dat.test[, c(predictors(results))])

# Add to performance data table
data.frame(dat.test$player,predictions,dat.test$NextSeasonPtsPerGame)
actuals_preds <- data.frame(cbind(actuals=dat.test$NextSeasonPtsPerGame, predicteds=predictions))
(correlation_accuracy <- cor(actuals_preds)) 
rb_performance[4,1] <- "kNN"
rb_performance[4,2] <- max(model_knn_rb$results[,"Rsquared"])
rb_performance[4,3] <- min(model_knn_rb$results[,"RMSE"])
rb_performance[4,4] <- correlation_accuracy[1,2]
rb_performance[4,5] <- correlation_accuracy[1,2]^2 
rb_performance[4,6] <- rmse(dat.test$NextSeasonPtsPerGame, predictions) 

# neural network with rfe features
model_nnet_rb <- train(dat.training[, c(predictors(results))],
                      dat.training[, "NextSeasonPtsPerGame"],
                      method = 'nnet',
                      preProcess = c('range'),
                      linout = TRUE,
                      skip = TRUE)

varImp(model_nnet_rb)

predictions <- predict(object=model_nnet_rb,
                       dat.test[, c(predictors(results))])

# Add to performance data table
data.frame(dat.test$player,predictions,dat.test$NextSeasonPtsPerGame)
actuals_preds <- data.frame(cbind(actuals=dat.test$NextSeasonPtsPerGame, predicteds=predictions))
(correlation_accuracy <- cor(actuals_preds)) 
rb_performance[5,1] <- "Neural Net"
rb_performance[5,2] <- max(model_nnet_rb$results[,"Rsquared"])
rb_performance[5,3] <- min(model_nnet_rb$results[,"RMSE"])
rb_performance[5,4] <- correlation_accuracy[1,2]
rb_performance[5,5] <- correlation_accuracy[1,2]^2 
rb_performance[5,6] <- rmse(dat.test$NextSeasonPtsPerGame, predictions) 

# Predict 2020 points
if(which.min(rb_performance$`RMSE Train`)==2){
  top_model_rb <- model_rf_rb
} else if(which.min(rb_performance$`RMSE Train`)==3){
  top_model_rb <- model_lm_rb
} else if(which.min(rb_performance$`RMSE Train`)==4){
  top_model_rb <- model_knn_rb
} else if(which.min(rb_performance$`RMSE Train`)==5){
  top_model_rb <- model_nnet_rb
} else {
  top_model_rb <- model_rf_rb
}

pred2019rb <- predict(object = top_model_rb,
                      datRB[datRB$Season==2019, c(predictors(results))])

# Final 2020 RB data table
PRED2019RB <- data.frame(datRB[datRB$Season==2019,]$player,"RB",pred2019rb*16)
colnames(PRED2019RB) <- c("Player", "Pos", "2020 Points")

#### WR ####
datWR <- dat[dat$position=="WR",]

# Remove pat columns (these happen far too infrequently)
datWR <- datWR[, -which(names(datWR) %in% c("rushYds100Games","rush40s","rushTd40s","rushTds","rushTa","tat","patConversions","patAttempts"))]

# Remove 2019 data to build model
datWRsmall <- datWR[datWR$Season!=2019,]

# Create index to split into training and test set
index <- createDataPartition(datWRsmall$NextSeasonPtsPerGame, p = 0.75, list = FALSE)

# Subset training set with index
dat.training <- datWRsmall[index,]

# Subset test set with index
dat.test <- datWRsmall[-index,]

## Look at correlation of features
# calculate correlation matrix
correlationMatrix <- cor(datWRsmall[, -which(names(dat.training) %in% c("player","team","position","Season","NextSeasonPts","NextSeasonPtsPerGame"))])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)

## Use Recursive Feature Elimination to select features for model
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results2 <- rfe(datWRsmall[, -which(names(dat.training) %in% c("player","team","position","Season","NextSeasonPts","NextSeasonPtsPerGame"))], datWRsmall[, "NextSeasonPtsPerGame"], sizes=c(1:29), rfeControl=control)
# summarize the results
print(results2)
# list the chosen features
predictors(results2)
# plot the results
plot(results2, type=c("g", "o"))

train_control <- trainControl(method="boot", number=100)
wr_performance <- data.frame()

# Train a model with all variables to compare against others
model_rf1_wr <- train(dat.training[, -which(names(dat.training) %in% c("player","team","position","Season","NextSeasonPts","NextSeasonPtsPerGame"))],
                     dat.training[, "NextSeasonPtsPerGame"],
                     method = 'rf',
                     family = "response",
                     trControl = train_control,
                     preProcess = c('range'))

varImp(model_rf1_wr)

predictions <- predict(object=model_rf1_wr,
                       dat.test[, -which(names(dat.test) %in% c("player","team","position","Season","NextSeasonPts","NextSeasonPtsPerGame"))])

# Add to performance data table
data.frame(dat.test$player,predictions,dat.test$NextSeasonPtsPerGame)
actuals_preds <- data.frame(cbind(actuals=dat.test$NextSeasonPtsPerGame, predicteds=predictions))
(correlation_accuracy <- cor(actuals_preds)) 
wr_performance[1,1] <- "RF (all features)"
wr_performance[1,2] <- max(model_rf1_wr$results[,"Rsquared"])
wr_performance[1,3] <- min(model_rf1_wr$results[,"RMSE"])
wr_performance[1,4] <- correlation_accuracy[1,2]
wr_performance[1,5] <- correlation_accuracy[1,2]^2 
wr_performance[1,6] <- rmse(dat.test$NextSeasonPtsPerGame, predictions) 
colnames(wr_performance) <- c("Model", "R^2 Train", "RMSE Train", "Correlation Test", "R^2 Test", "RMSE Test")

# Train a model with selected rfe variables
model_rf_wr <- train(dat.training[, c(predictors(results2))],
                     dat.training[, "NextSeasonPtsPerGame"],
                     method = 'rf',
                     family = "response",
                     trControl = train_control,
                     preProcess = c('range'))

varImp(model_rf_wr)

predictions <- predict(object=model_rf_wr,
                       dat.test[, c(predictors(results2))])

# Add to performance data table
data.frame(dat.test$player,predictions,dat.test$NextSeasonPtsPerGame)
actuals_preds <- data.frame(cbind(actuals=dat.test$NextSeasonPtsPerGame, predicteds=predictions))
(correlation_accuracy <- cor(actuals_preds)) 
wr_performance[2,1] <- "RF"
wr_performance[2,2] <- max(model_rf_wr$results[,"Rsquared"])
wr_performance[2,3] <- min(model_rf_wr$results[,"RMSE"])
wr_performance[2,4] <- correlation_accuracy[1,2]
wr_performance[2,5] <- correlation_accuracy[1,2]^2 
wr_performance[2,6] <- rmse(dat.test$NextSeasonPtsPerGame, predictions) 

# lm model with rfe features
model_lm_wr <- train(dat.training[, c(predictors(results2))],
                     dat.training[, "NextSeasonPtsPerGame"],
                     method = 'lm',
                     preProcess = c('range'))

varImp(model_lm_wr)
summary(model_lm_wr) 

predictions <- predict(object=model_lm_wr,
                       dat.test[, c(predictors(results2))])

# Add to performance data table
data.frame(dat.test$player,predictions,dat.test$NextSeasonPtsPerGame)
actuals_preds <- data.frame(cbind(actuals=dat.test$NextSeasonPtsPerGame, predicteds=predictions))
(correlation_accuracy <- cor(actuals_preds)) 
wr_performance[3,1] <- "Linear Regression"
wr_performance[3,2] <- max(model_lm_wr$results[,"Rsquared"])
wr_performance[3,3] <- min(model_lm_wr$results[,"RMSE"])
wr_performance[3,4] <- correlation_accuracy[1,2]
wr_performance[3,5] <- correlation_accuracy[1,2]^2 
wr_performance[3,6] <- rmse(dat.test$NextSeasonPtsPerGame, predictions) 

# knn model with rfe features
model_knn_wr <- train(dat.training[, c(predictors(results2))],
                      dat.training[, "NextSeasonPtsPerGame"],
                      method = 'knn',
                      preProcess = c('range'))

varImp(model_knn_wr)

predictions <- predict(object=model_knn_wr,
                       dat.test[, c(predictors(results2))])

# Add to performance data table
data.frame(dat.test$player,predictions,dat.test$NextSeasonPtsPerGame)
actuals_preds <- data.frame(cbind(actuals=dat.test$NextSeasonPtsPerGame, predicteds=predictions))
(correlation_accuracy <- cor(actuals_preds)) 
wr_performance[4,1] <- "kNN"
wr_performance[4,2] <- max(model_knn_wr$results[,"Rsquared"])
wr_performance[4,3] <- min(model_knn_wr$results[,"RMSE"])
wr_performance[4,4] <- correlation_accuracy[1,2]
wr_performance[4,5] <- correlation_accuracy[1,2]^2 
wr_performance[4,6] <- rmse(dat.test$NextSeasonPtsPerGame, predictions) 

# neural network with rfe features
model_nnet_wr <- train(dat.training[, c(predictors(results2))],
                       dat.training[, "NextSeasonPtsPerGame"],
                       method = 'nnet',
                       preProcess = c('range'),
                       linout = TRUE,
                       skip = TRUE)

varImp(model_nnet_wr)

predictions <- predict(object=model_nnet_wr,
                       dat.test[, c(predictors(results2))])

# Add to performance data table
data.frame(dat.test$player,predictions,dat.test$NextSeasonPtsPerGame)
actuals_preds <- data.frame(cbind(actuals=dat.test$NextSeasonPtsPerGame, predicteds=predictions))
(correlation_accuracy <- cor(actuals_preds)) 
wr_performance[5,1] <- "Neural Net"
wr_performance[5,2] <- max(model_nnet_wr$results[,"Rsquared"])
wr_performance[5,3] <- min(model_nnet_wr$results[,"RMSE"])
wr_performance[5,4] <- correlation_accuracy[1,2]
wr_performance[5,5] <- correlation_accuracy[1,2]^2 
wr_performance[5,6] <- rmse(dat.test$NextSeasonPtsPerGame, predictions) 

# Predict 2020 points
if(which.min(wr_performance$`RMSE Train`)==2){
  top_model_wr <- model_rf_wr
} else if(which.min(wr_performance$`RMSE Train`)==3){
  top_model_wr <- model_lm_wr
} else if(which.min(wr_performance$`RMSE Train`)==4){
  top_model_wr <- model_knn_wr
} else if(which.min(wr_performance$`RMSE Train`)==5){
  top_model_wr <- model_nnet_wr
} else {
  top_model_wr <- model_rf_wr
}

pred2019wr <- predict(object = top_model_wr,
                      datWR[datWR$Season==2019, c(predictors(results2))])

# Final 2020 WR data table
PRED2019WR <- data.frame(datWR[datWR$Season==2019,]$player,"WR",pred2019wr*16)
colnames(PRED2019WR) <- c("Player", "Pos", "2020 Points")

#### TE ####
datTE <- dat[dat$position=="TE",]

# Remove pat columns (these happen far too infrequently) and rush columns with all 0s
datTE <- datTE[, -which(names(datTE) %in% c("rushYds100Games","rush40s","rushTd40s","rushTds","rushTa","tat","patConversions","patAttempts"))]

# Remove 2019 data to build model
datTEsmall <- datTE[datTE$Season!=2019,]

# Create index to split into training and test set
index <- createDataPartition(datTEsmall$NextSeasonPtsPerGame, p = 0.75, list = FALSE)

# Subset training set with index
dat.training <- datTEsmall[index,]

# Subset test set with index
dat.test <- datTEsmall[-index,]

## Look at correlation of features
# calculate correlation matrix
correlationMatrix <- cor(datTEsmall[, -which(names(dat.training) %in% c("player","team","position","Season","NextSeasonPts","NextSeasonPtsPerGame"))])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)

## Use Recursive Feature Elimination to select features for model
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results3 <- rfe(datTEsmall[, -which(names(dat.training) %in% c("player","team","position","Season","NextSeasonPts","NextSeasonPtsPerGame"))], datTEsmall[, "NextSeasonPtsPerGame"], sizes=c(1:26), rfeControl=control)
# summarize the results3
print(results3)
# list the chosen features
predictors(results3)
# plot the results3
plot(results3, type=c("g", "o"))

train_control <- trainControl(method="boot", number=100)
te_performance <- data.frame()

# Train a model with all variables
model_rf1_te <- train(dat.training[, -which(names(dat.training) %in% c("player","team","position","Season","NextSeasonPts","NextSeasonPtsPerGame"))],
                     dat.training[, "NextSeasonPtsPerGame"],
                     method = 'rf',
                     family = "response",
                     trControl = train_control,
                     preProcess = c('range'))

varImp(model_rf1_te)

predictions <- predict(object=model_rf1_te,
                       dat.test[, -which(names(dat.test) %in% c("player","team","position","Season","NextSeasonPts","NextSeasonPtsPerGame"))])

# Add to performance data table
data.frame(dat.test$player,predictions,dat.test$NextSeasonPtsPerGame)
actuals_preds <- data.frame(cbind(actuals=dat.test$NextSeasonPtsPerGame, predicteds=predictions))
(correlation_accuracy <- cor(actuals_preds)) 
te_performance[1,1] <- "RF (all features)"
te_performance[1,2] <- max(model_rf1_te$results[,"Rsquared"])
te_performance[1,3] <- min(model_rf1_te$results[,"RMSE"])
te_performance[1,4] <- correlation_accuracy[1,2]
te_performance[1,5] <- correlation_accuracy[1,2]^2 
te_performance[1,6] <- rmse(dat.test$NextSeasonPtsPerGame, predictions) 
colnames(te_performance) <- c("Model","R^2 Train", "RMSE Train", "Correlation Test", "R^2 Test", "RMSE Test")

# Train a model with selected rfe variables
model_rf_te <- train(dat.training[, c(predictors(results3))],
                     dat.training[, "NextSeasonPtsPerGame"],
                     method = 'rf',
                     family = "response",
                     trControl = train_control,
                     preProcess = c('range'))

varImp(model_rf_te)

predictions <- predict(object=model_rf_te,
                       dat.test[, c(predictors(results3))])

# Add to performance data table
data.frame(dat.test$player,predictions,dat.test$NextSeasonPtsPerGame)
actuals_preds <- data.frame(cbind(actuals=dat.test$NextSeasonPtsPerGame, predicteds=predictions))
(correlation_accuracy <- cor(actuals_preds)) 
te_performance[2,1] <- "RF"
te_performance[2,2] <- max(model_rf_te$results[,"Rsquared"])
te_performance[2,3] <- min(model_rf_te$results[,"RMSE"])
te_performance[2,4] <- correlation_accuracy[1,2]
te_performance[2,5] <- correlation_accuracy[1,2]^2 
te_performance[2,6] <- rmse(dat.test$NextSeasonPtsPerGame, predictions) 

# lm model with rfe features
model_lm_te <- train(dat.training[, c(predictors(results3))],
                     dat.training[, "NextSeasonPtsPerGame"],
                     method = 'lm',
                     preProcess = c('range'))

varImp(model_lm_te)
summary(model_lm_te) 

predictions <- predict(object=model_lm_te,
                       dat.test[, c(predictors(results3))])

# Add to performance data table
data.frame(dat.test$player,predictions,dat.test$NextSeasonPtsPerGame)
actuals_preds <- data.frame(cbind(actuals=dat.test$NextSeasonPtsPerGame, predicteds=predictions))
(correlation_accuracy <- cor(actuals_preds)) 
te_performance[3,1] <- "Linear Regression"
te_performance[3,2] <- max(model_lm_te$results[,"Rsquared"])
te_performance[3,3] <- min(model_lm_te$results[,"RMSE"])
te_performance[3,4] <- correlation_accuracy[1,2]
te_performance[3,5] <- correlation_accuracy[1,2]^2 
te_performance[3,6] <- rmse(dat.test$NextSeasonPtsPerGame, predictions) 

# knn model with rfe features
model_knn_te <- train(dat.training[, c(predictors(results3))],
                      dat.training[, "NextSeasonPtsPerGame"],
                      method = 'knn',
                      preProcess = c('range'))

varImp(model_knn_te)

predictions <- predict(object=model_knn_te,
                       dat.test[, c(predictors(results3))])

# Add to performance data table
data.frame(dat.test$player,predictions,dat.test$NextSeasonPtsPerGame)
actuals_preds <- data.frame(cbind(actuals=dat.test$NextSeasonPtsPerGame, predicteds=predictions))
(correlation_accuracy <- cor(actuals_preds)) 
te_performance[4,1] <- "kNN"
te_performance[4,2] <- max(model_knn_te$results[,"Rsquared"])
te_performance[4,3] <- min(model_knn_te$results[,"RMSE"])
te_performance[4,4] <- correlation_accuracy[1,2]
te_performance[4,5] <- correlation_accuracy[1,2]^2 
te_performance[4,6] <- rmse(dat.test$NextSeasonPtsPerGame, predictions) 

# neural network with rfe features
model_nnet_te <- train(dat.training[, c(predictors(results3))],
                       dat.training[, "NextSeasonPtsPerGame"],
                       method = 'nnet',
                       preProcess = c('range'),
                       linout = TRUE,
                       skip = TRUE)

varImp(model_nnet_te)

predictions <- predict(object=model_nnet_te,
                       dat.test[, c(predictors(results3))])

# Add to performance data table
data.frame(dat.test$player,predictions,dat.test$NextSeasonPtsPerGame)
actuals_preds <- data.frame(cbind(actuals=dat.test$NextSeasonPtsPerGame, predicteds=predictions))
(correlation_accuracy <- cor(actuals_preds)) 
te_performance[5,1] <- "Neural Net"
te_performance[5,2] <- max(model_nnet_te$results[,"Rsquared"])
te_performance[5,3] <- min(model_nnet_te$results[,"RMSE"])
te_performance[5,4] <- correlation_accuracy[1,2]
te_performance[5,5] <- correlation_accuracy[1,2]^2 
te_performance[5,6] <- rmse(dat.test$NextSeasonPtsPerGame, predictions) 

# Predict 2020 points
if(which.min(te_performance$`RMSE Train`)==2){
  top_model_te <- model_rf_te
} else if(which.min(te_performance$`RMSE Train`)==3){
  top_model_te <- model_lm_te
} else if(which.min(te_performance$`RMSE Train`)==4){
  top_model_te <- model_knn_te
} else if(which.min(te_performance$`RMSE Train`)==5){
  top_model_te <- model_nnet_te
} else {
  top_model_te <- model_rf_te
}

pred2019te <- predict(object = top_model_te,
                      datTE[datTE$Season==2019, c(predictors(results3))])

# Final 2020 TE data table
PRED2019TE <- data.frame(datTE[datTE$Season==2019,]$player,"TE",pred2019te*16)
colnames(PRED2019TE) <- c("Player","Pos", "2020 Points")

#### FINAL OUTPUT ####
PRED2019ALL <- rbind(PRED2019RB,PRED2019WR,PRED2019TE)
PRED2019ALL <- PRED2019ALL[order(PRED2019ALL$`2020 Points`, decreasing = TRUE),]
PRED2019ALL <- PRED2019ALL[1:200, ]
bins <- bin(PRED2019ALL$`2020 Points`, nbins = 15, method = "cluster") #k-means to build tiers
Bin <- bins

# Final 2020 predictions output
PRED2019ALL <- cbind(PRED2019ALL,Bin)

# write.csv(PRED2019ALL, file = "2020_predictions.csv")


#### PLOTS ####
## Histogram plots comparing 2019 points to 2020 preds

# Top 50 RBs 2019
topRB2019 <- datRB[datRB$Season==2019, ]
topRB2019 <- topRB2019[order(topRB2019$fantasyPts, decreasing = TRUE), ]
topRB2019 <- topRB2019[1:50, ]

rb2019 <- ggplot(data = topRB2019, aes(x = fantasyPts)) +
  geom_histogram(aes(y=..density..), position="identity", binwidth = 25, alpha=0.75, fill = "turquoise")+
  geom_density(alpha=0.6)+
  scale_x_continuous(breaks = seq(0,500,50)) +
  ggtitle("2019 RB Season Point Totals") +
  labs(y = "Density", x = "Fantasy Points") +
  theme(plot.title = element_text(hjust = 0.5))

# Top 50 RBs 2020
topRB2020 <- PRED2019RB[order(PRED2019RB$`2020 Points`, decreasing = TRUE), ]
topRB2020 <- topRB2020[1:50, ]

rb2020 <- ggplot(data = topRB2020, aes(x = `2020 Points`)) +
  geom_histogram(aes(y=..density..), position="identity", binwidth = 25, alpha=0.75, fill = "orange")+
  geom_density(alpha=0.6)+
  scale_x_continuous(breaks = seq(0,500,50)) +
  ggtitle("2020 RB Season Point Predictions") +
  labs(y = "Density", x = "Fantasy Points") +
  theme(plot.title = element_text(hjust = 0.5))

# Top 100 WR 2019
topWR2019 <- datWR[datWR$Season==2019, ]
topWR2019 <- topWR2019[order(topWR2019$fantasyPts, decreasing = TRUE), ]
topWR2019 <- topWR2019[1:100, ]

wr2019 <- ggplot(data = topWR2019, aes(x = fantasyPts)) +
  geom_histogram(aes(y=..density..), position="identity", binwidth = 25, alpha=0.75, fill = "turquoise")+
  geom_density(alpha=0.6)+
  scale_x_continuous(breaks = seq(0,500,50)) +
  ggtitle("2019 WR Season Point Totals") +
  labs(y = "Density", x = "Fantasy Points") +
  theme(plot.title = element_text(hjust = 0.5))

# Top 100 WR 2020
topWR2020 <- PRED2019WR[order(PRED2019WR$`2020 Points`, decreasing = TRUE), ]
topWR2020 <- topWR2020[1:100, ]

wr2020 <- ggplot(data = topWR2020, aes(x = `2020 Points`)) +
  geom_histogram(aes(y=..density..), position="identity", binwidth = 25, alpha=0.75, fill = "orange")+
  geom_density(alpha=0.6)+
  scale_x_continuous(breaks = seq(0,500,50)) +
  ggtitle("2020 WR Season Point Predictions") +
  labs(y = "Density", x = "Fantasy Points") +
  theme(plot.title = element_text(hjust = 0.5))

# Top 50 TE 2019
topTE2019 <- datTE[datTE$Season==2019, ]
topTE2019 <- topTE2019[order(topTE2019$fantasyPts, decreasing = TRUE), ]
topTE2019 <- topTE2019[1:50, ]

te2019 <- ggplot(data = topTE2019, aes(x = fantasyPts)) +
  geom_histogram(aes(y=..density..), position="identity", binwidth = 25, alpha=0.75, fill = "turquoise")+
  geom_density(alpha=0.6)+
  scale_x_continuous(breaks = seq(0,500,50)) +
  ggtitle("2019 TE Season Point Totals") +
  labs(y = "Density", x = "Fantasy Points") +
  theme(plot.title = element_text(hjust = 0.5))

# Top 50 TE 2020
topTE2020 <- PRED2019TE[order(PRED2019TE$`2020 Points`, decreasing = TRUE), ]
topTE2020 <- topTE2020[1:50, ]

te2020 <- ggplot(data = topTE2020, aes(x = `2020 Points`)) +
  geom_histogram(aes(y=..density..), position="identity", binwidth = 25, alpha=0.75, fill = "orange")+
  geom_density(alpha=0.6)+
  scale_x_continuous(breaks = seq(0,500,50)) +
  ggtitle("2020 TE Season Point Predictions") +
  labs(y = "Density", x = "Fantasy Points") +
  theme(plot.title = element_text(hjust = 0.5))

cowplot::plot_grid(rb2019, rb2020,
                   wr2019, wr2020,
                   te2019, te2020,
                   nrow = 3)
