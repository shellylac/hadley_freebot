#Script to run twitter bot for HF titles

#load libraries
library(here)
library(tidyverse)
library(glue)
library(rtweet)

#Read in dataset
hadley_data <- read_csv(here("Data", "hadley_dataset.csv"))
#dataset of rows already tweeted
rows_tweeted <- read_csv(here("Data", "rows_tweeted.csv"))

#....................................
#Function to compose the tweet
#.....................................

#Function to randomly select row to tweet
row_select <- function(dataset){
  row_id <- sample(1:dim(dataset)[1], 1)
  
  while (row_id %in% rows_tweeted) {
    row_id <- sample(1:dim(dataset)[1], 1)
  }
  #append selected row_id to row_ids_read
  rows_tweeted <<- rbind(rows_tweeted, row_id)
  #Overwrite rows_tweeted dataset to update list of articles tweeted
  write_csv(rows_tweeted, here("Data", "rows_tweeted.csv"))
  return(row_id)
}

#Function to form twitter message
tweet_sentence <- function(dataset) {
  row_id <- row_select(dataset)
  title <- dataset$title[row_id]
  date <- dataset$pretty_date[row_id]
  url <- dataset$url[row_id]
  tweet <- glue("Love reading @HadleyFreeman Guardian articles? Then re-read {title} from {date}. {url}")
  return(tweet)
}

#Generate sentence to tweet
tweet <- tweet_sentence(hadley_data)

#Connect to Twitter (use case interactive within R)
# rtweet::create_token(
#   app = "Hadley Freebot",  # the name of the Twitter app
#   consumer_key = keyring::key_get("TWITTER_CONSUMER_API_KEY_hadleybot"),
#   consumer_secret = keyring::key_get("TWITTER_CONSUMER_API_SECRET_hadleybot"),
#   access_token = keyring::key_get("TWITTER_ACCESS_TOKEN_hadleybot"),
#   access_secret = keyring::key_get("TWITTER_ACCESS_TOKEN_SECRET_hadleybot")
#)

#Connect to Twitter (use case for Github Actions)
rtweet::create_token(
  app = "Hadley Freebot",  # the name of the Twitter app
  consumer_key = Sys.getenv("TWITTER_CONSUMER_API_KEY_HADLEYBOT"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET_HADLEYBOT"),
  access_token = Sys.getenv("TWITTER_ACCESS_TOKEN_HADLEYBOT"),
  access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET_HADLEYBOT")
)

# tweet it
post_tweet(status = glue("{tweet}"))

