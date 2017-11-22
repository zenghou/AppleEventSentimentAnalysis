###########################################
#### Analyze sentiment on a word level ####
###########################################

# unnest the words in each tweet
unnest <- df %>%
  unnest_tokens(IndividualWordInTweet, tweet)

# view top 5 rows to check
head(unnest)

# sentiment 
sentimentsCleaned  <- unnest %>% 
  # remove stop words 
  anti_join(stop_words, by = c("IndividualWordInTweet" = "word")) %>% 
  # get score for each word 
  inner_join(get_sentiments("afinn"), by = c("IndividualWordInTweet" = "word"))

# word cloud 
wrdCloud <- unnest %>% 
  # remove stop words 
  anti_join(stop_words, by = c("IndividualWordInTweet" = "word")) %>% 
  # get score for each word 
  inner_join(get_sentiments("afinn"), by = c("IndividualWordInTweet" = "word")) %>% 
  # sort according to word frequency
  count(IndividualWordInTweet, sort = TRUE) %>%
  with(wordcloud(IndividualWordInTweet, n, max.words = 150))

# comparison cloud 
compareCloud <- unnest %>%
  inner_join(get_sentiments("bing"), by = c("IndividualWordInTweet" = "word")) %>% 
  count(IndividualWordInTweet, sentiment, sort = TRUE ) %>% 
  # idk what acast does
  acast(IndividualWordInTweet ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("blue", "red"), max.words = 150)

# change date col from factor to character 
sentimentsCleaned$time <- as.character(sentimentsCleaned$time)
class(sentimentsCleaned$time) # check if conversion is correct

# group by date before aggregating
group_by(sentimentsCleaned, time)
minuteScore <- aggregate(sentimentsCleaned$score, FUN = mean, by = list(time = sentimentsCleaned$time))

# plot chart for sentiment score retrieved sententially v.s. by word
plot(minuteScore$x, xlab = "minute", ylab = "Average Sentiment Score", type = "l",  main ="Sentiment Score", col = "red")
lines(rateOfReturn$sentimentScore, col = "green")

# read data prices for Apple Shares
ApplePrices <- read.csv("ApplePrices.csv")
ApplePrices <- data.frame(price)

# clean up random NA values from idk where
ApplePrices[5:10] <- NULL
ApplePrices <- na.omit(ApplePrices)

# convert factor column to character vector
ApplePrices$date = as.character(ApplePrices$date)

# plot chart for sentiment score (by tweet) and stock price
plot(rateOfReturn$sentimentScore, xlab = "minute", ylab = "Average Sentiment Score", type = "l",  main ="Sentiment Score", col = "red")
lines(ApplePrices$open, col = "green")

# combine sentiment and prices into single df
sentimentAndPrice <- inner_join(ApplePrices, minuteScore, by = "date")
sentimentAndPrice

ggplot() + 
  geom_point(data = sentimentAndPrice, mapping = aes(x = date, y = open))