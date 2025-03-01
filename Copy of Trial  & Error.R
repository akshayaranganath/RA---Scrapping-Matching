---
  title: "Trial & Error"
author: "Akshaya"
date: "2023-07-23"
output: html_document
---
  
  # Libraries
  library(tidyverse)
library(jsonlite)

base_url <- "https://scholar.google.com/scholar?q=child+parent+school+education+program+Family+participation+enroll"

get_titles_from_page <- function(url) {
  html_content <- read_html(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"))
  titles <- html_content %>%
    html_nodes(".gs_rt a") %>%
    html_text()
  return(titles)
}
CCT_Studies <- read_csv("C:\\Users\\aksha\\OneDrive - The Pennsylvania State University\\Desktop\\Penn State\\Academics\\Summer\\RA\\Google Scholar Code\\CCT_Studies.csv")
matched_titles <- c()
page <- 0
while (TRUE) {
  url <- paste0(base_url, "&start=", page)
  titles <- get_titles_from_page(url)
  
  if (length(titles) == 0) {
    break  # No more results
  }
  matched_titles <- c(matched_titles, titles[titles %in% CCT_Studies$title])
  
  page <- page + 10  # Increase the start index for the next page
}
print(matched_titles)
# Read the CSV file
CCT_Studies <- read_csv("C:\\Users\\aksha\\OneDrive - The Pennsylvania State University\\Desktop\\Penn State\\Academics\\Summer\\RA\\Google Scholar Code\\CCT_Studies.csv")

# Define the search function
searcher <- function(k, df) {
  a <- "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C39&q=child+parent+school+education+program+Family+participation+enroll&btnG=child%20parent%20school%20education%20program%20Family%20participation%20enroll&offset=0&limit=100&fields=title"
  con <- file(a) 
  open(con)
  objects <- list()
  index <- 1
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    objects[[index]] <- fromJSON(line)
    index <- index + 1
  } 
  close(con)
  objects <- unlist(objects, recursive = FALSE)
  total <- objects$total
  
  offset <- 0
  list <- list()
  i <- 1
  
  while (total > offset) {
    a <- "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C39&q=child+parent+school+education+program+Family+participation+enroll&btnG=child%20parent%20school%20education%20program%20Family%20participation%20enroll&offset=0&limit=100&fields=title"
    con <- file(a) 
    open(con)
    objects <- list()
    index <- 1
    while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
      objects[[index]] <- fromJSON(line)
      index <- index + 1
    } 
    close(con)
    list <- c(list, objects)
    offset <- offset + 100
    i <- i + 1
  }
  
  list <- unlist(list, recursive = FALSE)
  list <- list[names(list) != "total"]
  list <- list[names(list) != "offset"]
  list <- list[names(list) != "next"]
  df <- Reduce(full_join, list)
  df <- df[-1]
  
  c <- 1
  match <- c()
  while (c < 29) {
    temp <- which(df$title %in% df[c, "title"])
    match <- c(match, temp)
    c <- c + 1
  }
  
  results <- list(total = total, match = match, df = df)
  return(results)
}

# Search criteria and other variables
k <- 'child parent school education program Family participation enroll'

# Call the searcher function with the given keywords and CCT_Studies data frame
search_results <- searcher(k, CCT_Studies)

# Print the total number of papers found
print(paste("Total papers found:", search_results$total))

# Print the indices of the matching papers
print("Indices of matching papers:")
print(search_results$match)

# Looping to Print Matched Titles
title <- character(length(search_results$match))
for (i in 1:length(search_results$match)) {
  row_no <- search_results$match[i]
  title[i] <- search_results$df[row_no, "title"]
}

print(title)
