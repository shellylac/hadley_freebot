#Script to run twitter bot for HF titles

#load libraries
library(here)
library(tidyverse)
library(glue)
library(rtweet)

#Read in dataset
hadley_data <- read_csv(here("Data", "hadley_dataset.csv"))

#....................................
#Function to compose the tweet
#.....................................

#indicator of dataset rows already tweeted
row_ids_read <- 0

#Function to randomly select row to tweet
row_select <- function(dataset){
  row_id <- sample(1:dim(dataset)[1], 1)
  
  while (row_id %in% row_ids_read) {
    row_id <- sample(1:dim(dataset)[1], 1)
  }
  #append selected row_id to row_ids_read
  row_ids_read <<- append(row_ids_read, row_id)
  return(row_id)
}

#Function to form twitter message
tweet_sentence <- function(dataset) {
  row_id <- row_select(dataset)
  title <- dataset$title[row_id]
  date <- dataset$pretty_date[row_id]
  url <- dataset$url[row_id]
  tweet <- glue("Love reading @HadleyFreeman Guardian articles? Then re-read \"{title}\" from {date}. {url}")
  return(tweet)
}

#Generate sentence to tweet
tweet <- tweet_sentence(hadley_data)

#Connect to Twitter
rtweet::create_token(
  app = "hadley_freebot",  # the name of the Twitter app
  consumer_key = keyring::key_get("TWITTER_CONSUMER_API_KEY_bot"),
  consumer_secret = keyring::key_get("TWITTER_CONSUMER_API_SECRET_bot"),
  access_token = keyring::key_get("TWITTER_ACCESS_TOKEN_bot"),
  access_secret = keyring::key_get("TWITTER_ACCESS_TOKEN_SECRET_bot")
)

rtweet::post_tweet(status = "This is a test tweet.")

# tweet it
post_tweet(status = glue("{tweet}"))


