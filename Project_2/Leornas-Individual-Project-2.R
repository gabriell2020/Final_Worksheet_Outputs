#Individual Project 2

#Insert Libraries
library("twitteR") 
library(dplyr)
library(tidyr)
library("plotly")
library(ggplot2)
library(RColorBrewer)
library(tidytext)
library(rtweet)
library(tm)
library(slam)
library(wordcloud)
library(wordcloud2)
library(corpus)


#Extract from twitter using your developer's credentials. 
#Choose any keyword you want.

#Set-up credentials
CONSUMER_SECRET <-"2vmVGAlukrEnyO9f20nEtJfv96GEFBFbe7e8CuwFGl9ozY9kSi"
CONSUMER_KEY <-"l5CSikp1YC2oBdmLh08DDMDoc"
ACCESS_SECRET <- "dNFeJKTMdBixOaJBn6YXBsOCaybvBWBw8YxvnuTBgeApu" 
ACCESS_TOKEN <- "1596107281610858498-FjtGw5XzODkRVyfEGvjQX5xo8bPIF4" 

#connect to twitter app
setup_twitter_oauth(consumer_key = CONSUMER_KEY,
                    consumer_secret = CONSUMER_SECRET,
                    access_token = ACCESS_TOKEN,
                    access_secret = ACCESS_SECRET)
#Getting a data
#it would take few minutes to load which depend the number of data you need
#but when you already save this data as a file you can skip this part.
trendTweets2 <- searchTwitter("#Anime -filter:retweets",
                             n = 10000,
                             lang = "en",
                             since = "2022-11-20",
                             until = "2022-12-8",
                             retryOnRateLimit=120)

trendTweets2

# CONVERTING LIST DATA TO DATA FRAME.
AnimetweetsDF <- twListToDF(trendTweets2)

# SAVE DATA FRAME FILE.
save(AnimetweetsDF,file = "AnimeTweetsDF.Rdata")

# LOAD DATA FRAME FILE.
load(file = "AnimeTweetsDF.Rdata")

# CHECKING FOR MISSING VALUES IN A DATA FRAME.
sap_data <- sapply(AnimetweetsDF, function(x) sum(is.na(x)))
sap_data

#Tweets
# SUBSETTING USING THE dplyr() PACKAGE.
tweets <- AnimetweetsDF %>%
  select(screenName,text,created, isRetweet) %>% filter(isRetweet == FALSE)
tweets

# GROUPING THE DATA CREATED. 
tweets %>%  
  group_by(1) %>%  
  summarise(max = max(created), min = min(created))

crt_data <- tweets %>%  mutate(Created_At_Round = created %>% round(units = 'hours') %>% as.POSIXct())
crt_data

mn <- tweets %>% pull(created) %>% min()
mn 
mx <- tweets %>% pull(created) %>% max()
mx

# Plot on tweets by time using the library(plotly) and ggplot().
plt_data <- ggplot(crt_data, aes(x = Created_At_Round)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "right") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

plt_data %>% ggplotly()


#Retweets

sub_tweets <- AnimetweetsDF %>%
  select(screenName,text,created, isRetweet) %>% filter(isRetweet == TRUE)
sub_tweets


sub_tweets %>%  
  group_by(1) %>%  
  summarise(max = max(created), min = min(created))

crt2 <- sub_tweets %>%  mutate(Created_At_Round = created %>% round(units = 'hours') %>% as.POSIXct())
crt2

mn <- sub_tweets %>% pull(created) %>% min()
mn 
mx <- sub_tweets %>% pull(created) %>% max()
mx

# Plot on tweets by time using the library(plotly) and ggplot().
plt_data <- ggplot(crt2, aes(x = Created_At_Round)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "right") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

plt_data %>% ggplotly()
