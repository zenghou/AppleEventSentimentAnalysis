library(NLP)
library(openNLP)
library(magrittr)
library(dplyr)
library(stringr)
library(tidytext)
library(wordcloud)
library(reshape2)
library(ggplot2)

# clear global environemtn
rm(list=ls())

# original dataframe 
df <- read.table("AllTweets.txt", sep ="\t", comment.char='', quote='', header= TRUE)

# convert tweet column from factor to character 
df$tweet <- as.character(df$tweet)

# unnest the words in each tweet
df <- df %>%
  unnest_tokens(IndividualWordInTweet, tweet)

# view top 5 rows to check
head(df)

# sentiment 
sentimentsCleaned  <- df %>% 
  # remove stop words 
  anti_join(stop_words, by = c("IndividualWordInTweet" = "word")) %>% 
  # get score for each word 
  inner_join(get_sentiments("afinn"), by = c("IndividualWordInTweet" = "word"))

# word cloud 
wrdCloud <- df %>% 
  # remove stop words 
  anti_join(stop_words, by = c("IndividualWordInTweet" = "word")) %>% 
  # get score for each word 
  inner_join(get_sentiments("afinn"), by = c("IndividualWordInTweet" = "word")) %>% 
  # sort according to word frequency
  count(IndividualWordInTweet, sort = TRUE) %>%
  with(wordcloud(IndividualWordInTweet, n, max.words = 150))

# comparison cloud 
compareCloud <- df %>%
  inner_join(get_sentiments("bing"), by = c("IndividualWordInTweet" = "word")) %>% 
  count(IndividualWordInTweet, sentiment, sort = TRUE ) %>% 
  # idk what acast does
  acast(IndividualWordInTweet ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("blue", "red"), max.words = 150)

# change date col from factor to character 
sentimentsCleaned$date <- as.character(sentimentsCleaned$date)
class(sentimentsCleaned$date) # check if conversion is correct

# get time until minute
sentimentsCleaned$date <- substr(sentimentsCleaned$date, 12,16)
head(sentimentsCleaned)

# group by date before aggregating
group_by(sentimentsCleaned, date)
minuteScore <- aggregate(sentimentsCleaned$score, FUN = mean, by = list(date = sentimentsCleaned$date))

# plot chart
plot(minuteScore$x, xlab = "minute", ylab = "Average Sentiment Score", type = "l")

# read data prices for Apple Shares
ApplePrices <- read.csv("ApplePrices.csv")
ApplePrices <- data.frame(price)

# clean up random NA values from idk where
ApplePrices[5:10] <- NULL
ApplePrices <- na.omit(ApplePrices)

# convert factor column to character vector
ApplePrices$date = as.character(ApplePrices$date)

# combine sentiment and prices into single df
sentimentAndPrice <- inner_join(ApplePrices, minuteScore, by = "date")
sentimentAndPrice

ggplot() + 
  geom_point(data = sentimentAndPrice, mapping = aes(x = date, y = open))
