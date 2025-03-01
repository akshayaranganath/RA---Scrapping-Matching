---
  title: "Semantic Scholar Editing Version"
author: "Akshaya"
date: "2023-07-23"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


---
  ##READ ME
  Instructions:
  Step 1. Run code
Step 2. Adjust keywords: k<-'[insert search here]' (alternatively, type keywords directly into function as string)
Step 3. Run function for your keywords
Step 4. View the output to see the how many studies the search returned and the number of matches
Step 5. Download the dataframe as a csv
```{r setup, include=FALSE}
rm(list=ls())
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(jsonlite)
library(data.table)
library(readr)
library(readr)
CCT_Studies <- read_csv("C:\\Users\\aksha\\OneDrive - The Pennsylvania State University\\Desktop\\Try 4\\CCT_Studies.csv")
```
#search criteria and other variables
```{r}
k<- 'conditional+cash+transfer+child+parent+school+education+program+family+participation+enroll'
offset<-0
list<-c()
i<-1
```

```{r}
searcher<-function(k, CCT_Studies.csv){
  a<-paste0('https://api.semanticscholar.org/graph/v1/paper/search?query=',k,'&offset=',offset,'&limit=100&fields=title')
  con <- file(a) 
  open(con)
  objects <- list()
  index <- 1
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    objects[[index]] <- fromJSON(line)
    index <- index + 1
  } 
  close(con)
  objects<-unlist(objects, recursive = F)
  total<-objects$total
  while (total>offset){
    {a<-paste0('https://api.semanticscholar.org/graph/v1/paper/search?query=',k,'&offset=',offset,'&limit=100&fields=title')
    con <- file(a) 
    open(con)
    objects <- list()
    index <- 1
    while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
      objects[[index]] <- fromJSON(line)
      index <- index + 1
    } 
    close(con)
    list<-c(list, objects)
    }
    offset<-offset+100
    i<-i+1
  }
  list<-unlist(list, recursive = F)
  list<-list[names(list) %in% "total"==F]
  list<-list[names(list) %in% "offset"==F]
  list<-list[names(list) %in% "next"==F]
  df<-Reduce(full_join,list)
  df<-df[-1]
  c<-1
  match<-c()
  while (c<29){
    temp<-which(df$title %in% CCT_Studies[c,])
    match<-c(match,temp)
    c<-c+1
  }
  print(total)
  print(match)
  df<-mutate(df, search=k, total=total)
}
```
