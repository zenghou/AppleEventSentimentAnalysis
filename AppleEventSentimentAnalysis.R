library(dplyr)
library(stringr)
library(tidytext)
library(wordcloud)
library(reshape2)
library(ggplot2)
library(syuzhet)
library(sentimentr)
library(tidyr)
library(wordcloud2)
library(rsconnect)

# clear global environemtn
rm(list=ls())

######################################
#### Clean up and manipulate data ####
######################################
  
# original dataframe 
df <- read.table("AllTweets.txt", sep ="\t", comment.char='', quote='', header= TRUE)

# convert tweet column from factor to character 
df$tweet <- as.character(df$tweet)

# remove hashtags
df$tweet <- gsub('#.*', "", df$tweet)

# remove urls from tweets
df$tweet <- gsub('http.* *', "", df$tweet)

# remove picure links
df$tweet <-gsub('pic.* *', "", df$tweet)

# remove emoji from string (adapted from stackoverflow solution)
# https://stackoverflow.com/questions/11129107/removing-non-alpanumeric-characters-from-an-ordered-collection-of-objects-list
df$tweet <- gsub("[^a-zA-Z0-9.,'!]"," ", df$tweet)

df <- df[df$tweet != " " || df$tweet != "" || !is.na(df$tweet) ]

# remove trailing leading white spaces and multiple spaces 
df$tweet <- gsub("\\s+", " ", str_trim(df$tweet))

# get dataframe of SentimentScores 
sentimentScores <- get_sentiment(df$tweet)
sentimentScores <- data.frame(sentimentScores)
sentimentScores$sentimentScores <- as.numeric(sentimentScores$sentimentScores)

# combine two dataframes
df <- cbind(df, sentimentScores)

# get only time (HH:MM) for date column
df$date <- as.character(df$date)
df$date <- substr(df$date, 12, 16)
df <- df[df$tweet != " " || df$tweet != "" || !is.na(df$tweet) ]

# rename cols
colnames(df) <- c('id', 'numRetweets', 'tweet', 'time','hashtag', 'sentimentScore')

# get average sentiment score for each minute
minuteSentimentScoreBySentence <- aggregate(df$sentimentScore, FUN = mean, by = list(date = df$time))

# get average of all average sentiment score 
average <- mean(minuteSentimentScoreBySentence$x)

# boolean col (above average or not)
minuteSentimentScoreBySentence <- minuteSentimentScoreBySentence %>% 
  mutate(aboveAverage = x >= average)

# rename cols
colnames(minuteSentimentScoreBySentence) <- c('time', 'sentimentScore', 'isAboveAverage')

# create event vector 
event <- rbind(data.frame(event = rep("Introduction", 10)), data.frame(event = rep("General", 8))) %>% 
  rbind(data.frame(event = rep("AppleWatch", 21))) %>% 
  rbind(data.frame(event = rep("AppleTV", 13))) %>% 
  rbind(data.frame(event = rep("iPhone8", 24))) %>% 
  rbind(data.frame(event = rep("iPhoneX", 34))) %>%
  rbind(data.frame(event = rep("Ending", 4))) %>% 
  rbind(data.frame(event = rep("Post-event", 6)))

minuteSentimentScoreBySentence <- cbind(minuteSentimentScoreBySentence, event)  

# plotting
ggplot(minuteSentimentScoreBySentence, aes(time, sentimentScore)) + 
  geom_bar(stat = "identity", aes(fill= isAboveAverage)) +

  labs(x = "Time", y = "Average Sentiment Score", 
       subtitle = "An average sentiment score of tweets per minute",
       fill = "Above Average") + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
        # plot.background = element_rect(fill = 'green', colour = 'red')))

# basic plot
# plot(minuteSentimentScore$x, xlab = "minute", ylab = "Average Sentiment Score", type = "l")

################################################
#### Word cloud based on event segmentation ####
################################################

# convert date col into numeric
tempDf <- separate(df, time, c("hour", "minute"), sep=":") %>%
 unite("time", c(hour, minute), sep="")
tempDf$time <- as.numeric(tempDf$time)

# 1:00 - 1:09 Introduction
all <- tempDf

introduction <- filter(tempDf, time >= 100 & time <=109) 

# 1:10 - 1:17 General
general <- filter(tempDf, time >= 110 & time <=117) 

# 1:18 - 1:38 Apple Watch
appleWatch <- filter(tempDf, time >= 118 & time <=138) 

# 1:39 - 1:51 Apple TV
appleTV <- filter(tempDf, time >= 139 & time <=151) 

# 1:52 - 2:15 iPhone8
iPhone8 <- filter(tempDf, time >= 152 & time <=215) 

# 2:16 - 2:49 iPhoneX
iPhoneX <- filter(tempDf, time >= 216 & time <=249) 

# 2:50 - 2:53 Ending 
ending <- filter(tempDf, time >= 250 & time <=253) 

listOfWordCloud <- list(introduction, general, appleWatch,
                        appleTV, iPhone8, iPhoneX, ending)

# function to generate word cloud of n words given dataframe
generateWordCloud <- function(df) {
  
  # custom stop words that are related to apple or the product
  customStopWords <- data.frame(word = c("apple", "Apple"))
                                         # "iphone","iPhone","iPhone8", "iphone8",
                                         # "iphoneX", "iPhoneX", "iphonex", "iphoneX", "phone",
                                         # "event", ""))
  
  # change to dataframe
  customStopWords$word <- as.character(customStopWords$word)
  
  wordCount <- df %>% 
    unnest_tokens(IndividualWords, tweet) %>% 
    anti_join(stop_words, by = c("IndividualWords" = "word")) %>% 
    anti_join(customStopWords, by = c("IndividualWords" = "word")) %>% 
    count(IndividualWords, sort = TRUE) 
  
  # plot wordcloud2
  wordcloud2(wordCount, size = 1, minSize = 0.5, gridSize =  0,
             fontFamily = 'Helvetica Neue', fontWeight = '200',
             color = 'white', backgroundColor = "black",
             minRotation = -pi/2, maxRotation = pi/2, shuffle = FALSE,
             rotateRatio = 0.3, shape = 'circle', ellipticity = 0.65)
             # widgetsize = NULL, figPath = NULL, hoverFunction = NULL)
}

################################
#### Analyze rate of return ####
################################

rateOfReturn <- read.table('rateOfReturn.txt', sep = '\t',header = TRUE)
colnames(rateOfReturn) <- c('time', 'openingPrice', 'r', 'r1', 'r2', 'r3', 'volume', 'sentimentScore', 'event')
rateOfReturn$time <- minuteSentimentScoreBySentence$time
rateOfReturn$event <- minuteSentimentScoreBySentence$event
rateOfReturn <- cbind(rateOfReturn, minuteSentimentScoreBySentence[,3])
colnames(rateOfReturn) <- c('time', 'openingPrice', 'r', 'r1', 'r2', 'r3', 'volume', 'sentimentScore', 'event', 'isAboveAverage')
rateOfReturn <- rateOfReturn[1:114,]

rSquaredValues <- c(0.0181, 0.0827, 0.1282, 0.0717)

# plotting
ggplot(rateOfReturn, aes(x = time)) +
  geom_line(stat = "identity", aes(y = sentimentScore, col = 'Sentiment Score', group = 1)) +
  geom_line(aes(y = rateOfReturn[,3]*100, col = 'Rate of Return', group = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~.*0.01, name = "Rate of Return (%)")) +
  labs(x = "Time", y = "Average Sentiment Score", 
       subtitle = "An average sentiment score of tweets per minute",
       fill = "Above Average") + 
  scale_color_manual(labels = c('Sentiment Score', 'Rate of Return'),
                     values = c('Sentiment Score'='#00ba38', 'Rate of Return' = '#f8766d')) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())


y2 <- list(
  tickfont = list(color = "black"),
  overlaying = "y",
  side = "right",
  title = 'Rate of Return (%)',
  range = c(-0.45, 0.45)
)

p <- plot_ly() %>% 
  add_lines(x = ~rateOfReturn$time, y = ~rateOfReturn$sentimentScore, name = 'Sentiment Score') %>% 
  add_lines(x = ~rateOfReturn$time, y = ~100*rateOfReturn[,3], name = 'Rate of Return') %>% 
  layout(
    title = "Average Sentiment Score v.s. Rate of Return",
    yaxis2 = y2,
    xaxis = list(title = "time"),
    yaxis = list(title = "Average Sentiment Score Per Min",range = c(-0.45, 0.45))
  )