library(tidyr)
library(dplyr)
library(tidytext)
library(rvest)

beer <- read.csv("brewery_beers.csv", stringsAsFactors=F)
beer$Review_Text <- NA

for(p in seq_along(beer$Beer_URL)){
  if(beer$Reviews[p]<5 | is.na(beer$Reviews[p])) next
  review_text <- ""
  pgst <- 0
  while(nchar(review_text) < 2000) { 
    reviews <- read_html(paste("https://www.beeradvocate.com", beer$Beer_URL[p], "?view=beer&sort=&start=", pgst, sep="")) %>%
      html_nodes(xpath="//div[@id='rating_fullview_content_2']/text()") %>%
      html_text()
    if(length(reviews)==0) break
    reviews <- paste(reviews[!grepl("rDev", reviews)], collapse=" ")
    review_text <- paste(review_text, reviews, collapse=" ")
    pgst <- pgst + 25
  }
  beer$Review_Text[p] <- review_text
  these_names <- c(strsplit(beer$Name[p], " ")[[1]], strsplit(beer$Brewery[p], " ")[[1]])
  beer$Review_Text[p] <- gsub(paste(these_names, collapse="|"), "", beer$Review_Text[p])
  print(p)
  write.csv(beer, "beer_review_progress.csv")
}

write(beer[,c(11,12)], "brewery_beers_reviews.csv", row.names=F)
