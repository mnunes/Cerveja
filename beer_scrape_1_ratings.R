library(XML)
library(RCurl)
library(tm)
library(caret)

mega_beers <- NULL
states <- c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO',
            'CT', 'DE','DC','FL','GA','HI','ID',
            'IL','IN','IA','KS','KY','LA','ME',
            'MD','MA','MI','MN','MS','MO','MT',
            'NE','NV','NH','NJ','NM','NY','NC',
            'ND','OH','OK','OR','PA','RI','SC',
            'SD','TN','TX','UT','VT','VA','WA',
            'WV','WI','WY')

locations <- NULL
for(st in seq_along(states)){
  # Get a list of brewery names and URLs.
  for(pg in seq(0, 800, by=20)){
    URL <- paste("https://www.beeradvocate.com/place/list/?start=", pg, "&&s_id=", states[st], "&brewery=Y&sort=name", sep="")
    html <- htmlTreeParse(getURL(URL, followlocation=TRUE), useInternal=TRUE)
    locs <- xpathSApply(html, "//table//tr//td//a", xmlGetAttr, "href")
    locs <- locs[grepl("^/beer/profile", locs)]
    if(length(locs) > 0){
      locs2 <- data.frame(state=states[st], url=locs, name=xpathSApply(html, "//table//tr//td//a//b", xmlValue))
      locations <- rbind(locations, locs2)
    } else {
      break
    }
  }
}

# Get a list of beer names and URLs by brewery.
all_beers <- NULL
for(u in seq_along(locations$url)){
  URL <- paste("https://www.beeradvocate.com", locations$url[u], sep="")
  html <- htmlTreeParse(getURL(URL, followlocation=TRUE), useInternal=TRUE)
  beers <- xpathSApply(html, "//table[@class='sortable']//tr/td", xmlValue)
  beer_urls <- xpathSApply(html, "//table[@class='sortable']//tr//td//a", xmlGetAttr, "href")
  beer_urls <- beer_urls[grepl("^/beer/profile", beer_urls)]
  beer_urls <- beer_urls[!grepl("ba=bros", beer_urls)]
  if(length(beers) > 0){
    rid <- c(seq(6, length(beers), 7), seq(7, length(beers), 7))
    beers <- beers[-rid]
    beers <- data.frame(matrix(beers, ncol=5, byrow=T), stringsAsFactors=F)
    colnames(beers) <- c("Name", "Style", "ABV", "Ratings", "Avg")
    beers$Beer_URL <- beer_urls
    beers$State <- locations$state[u]
    beers$Brewery <- locations$name[u]
    all_beers <- rbind(all_beers, beers)
  }
}

# Keep only beers with at least 1 rating.
for(g in c(3:5)) all_beers[,g] <- as.numeric(all_beers[,g])
all_beers <- all_beers[all_beers$Ratings >= 10, ] 

# Get score, review number and pdev for each beer. 
all_beers$score <- NA
all_beers$reviews <- NA
all_beers$pdev <- NA
for(o in seq_along(all_beers$Beer_URL)){
  URL <- paste("https://www.beeradvocate.com", all_beers$Beer_URL[o], sep="")
  html <- htmlTreeParse(getURL(URL, followlocation=TRUE), useInternal=TRUE)
  all_beers$score[o] <- xpathSApply(html, "//div[@class='break']//span[@class='BAscore_big ba-score']", xmlValue)
  all_beers$reviews[o] <- xpathSApply(html, "//div[@id='item_stats']//span[@class='ba-reviews']", xmlValue)
  all_beers$pdev[o] <- gsub("\\\n|\\\t|\\%", "", xpathSApply(html, "//div[@id='item_stats']//span[@class='ba-pdev']", xmlValue))
}     


for(g in c(9:11)) all_beers[,g] <- as.numeric(all_beers[,g])
all_beers$Brewery_URL <- sapply(all_beers$Beer_URL, function(x) paste(strsplit(x, "/")[[1]][1:4], collapse="/"))
for(u in seq_along(locs$url)){
  all_beers$State[all_beers$Brewery_URL %in% locs$url[u]] <- locs$state[u]
}

write(all_beers, "brewery_beers.csv", row.names=F)
