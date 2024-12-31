## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo =TRUE)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
if(!require("dplyr")) {
  install.packages("dplyr")
}
if(!require("tidytext")) {
  install.packages("tidytext")
}
if(!require("tidyr")) {
  install.packages("tidyr") 
}
if(!require("lubridate")) {
  install.packages("lubridate")
}
if(!require("ggplot2")) {
  install.packages("ggplot2")
}
library(dplyr)
library(tidytext)
library(tidyr) 
library(lubridate)
library(ggplot2)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
data <- read.csv("https://raw.githubusercontent.com/Khoagoodkid/datasets/refs/heads/main/datasets/Climate%20change_2022-1-17_2022-7-19.csv")

head(data)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
data <- data %>%
  filter(!is.na(Embedded_text))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
tokenized_data <- data %>%
  unnest_tokens(word, Embedded_text)
head(tokenized_data[c("UserScreenName", "word")])


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
word_counts <- tokenized_data %>%
  count(word, sort = TRUE)
head(word_counts)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#data("stop_words")
cleaned_tokens <- tokenized_data %>%
  anti_join(stop_words, by = "word")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
word_counts <- cleaned_tokens %>%
  count(word, sort = TRUE)
head(word_counts)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------

sentiment_tokens <- cleaned_tokens %>%
  inner_join(get_sentiments("bing"), by = "word")

head(sentiment_tokens[c("UserScreenName", "word", "sentiment")])


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
sentiment_scores <- sentiment_tokens %>%
  count(UserScreenName, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(Sentiment_Score = positive - negative)
head(sentiment_scores)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
tokenized_data <- tokenized_data %>%
  mutate(Date = as.Date(Timestamp))

head(tokenized_data[c("Timestamp", "Date")])


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
sentiment_trend <- tokenized_data %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(Date) %>%
  summarise(Sentiment_Score = sum(ifelse(sentiment == "positive", 1, -1)))

head(sentiment_trend)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(sentiment_trend, aes(x = Date, y = Sentiment_Score)) +
  geom_line() +
  labs(title = "Sentiment Trend Over Time", x = "Date", y = "Sentiment Score")


