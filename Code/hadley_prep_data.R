#Script to get Hadley Freeman article titles and URLs

#install keyring package and save guardian api key
#library(keyring)
#keyring::key_set("GUARDIAN_API_KEY")

#Load libraries
#install.packages("remotes")
#remotes::install_github("news-r/guardian") # github guardian package
#Set the API key for the session
library(guardian)
guardian_key(keyring::key_get("GUARDIAN_API_KEY"))
#> API key loaded!
library(here)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

## search for Hadley Freeman articles
hadley_search <- gd_search("Hadley AND Freeman", `query-fields`="byline", 
                            `show-fields`="byline", pages = 250) #section="commentisfree",

#Query the Guardian API for the results
hadley_results <- gd_call(hadley_search)
#dim(hadley_results)

#Filter to include on articles with solo byline
#hadley_only <- hadley_results %>% filter(nchar(fields[[1]]) >= 14)
hadley_only <- hadley_results %>% filter(nchar(fields[[1]]) > 13 & nchar(fields[[1]]) < 20)

#...................................................................
#Text preprocessing
#...................................................................
hadley_results_clean <- hadley_only %>% 
  as_tibble() %>% 
  ##strip all non-ascii characters from titles
  mutate(title = gsub('[^\x20-\x7E]', "", webTitle)) %>% 
  #remove HF name and whitespace
  mutate(title = str_replace(title, "\\|\\sHadley Freeman", "")) %>% 
  mutate(title = str_replace(title, "Hadley Freeman:", "")) %>%  
  mutate(title = trimws(title, "both")) %>% 
  #Get date in format for tweeting
  mutate(date = as.Date(webPublicationDate)) %>% 
  mutate(pretty_date = format(date, format = "%b %Y")) %>% 
  select(title, date, pretty_date, webUrl)

#Shorten URLs to add as links to tweets
#From here: https://www.listendata.com/2021/06/how-to-shorten-urls-with-r.html
ShortURL <- function(link, linkPreview = FALSE) {
  api <- if(linkPreview) {"http://v.gd/create.php?format=json"} else {"http://is.gd/create.php?format=json"}
  query <- list(url = link)
  request <- httr::GET(api, query = query)
  content <- httr::content(request, as = "text", encoding = "utf-8")
  result <- jsonlite::fromJSON(content)
  return(result)
}

short_urls <- sapply(hadley_results_clean$webUrl, FUN = ShortURL, USE.NAMES = F)
url <- unlist(unname(short_urls))
hadley_results_urls <- cbind(hadley_results_clean, url) %>% 
  select(-webUrl) %>% 
  #Add column to indicate when a title has been tweeted
  mutate(been_tweeted = 0)

#Save
write_csv(hadley_results_urls, file = here("Data", "hadley_dataset.csv"))
