
#library(jsonlite)
#install.packages("request") call api service
#library(request)

## sentiment analisys
#install.packages("NLP")
library(NLP)
#install.packages("syuzhet")
library(syuzhet)

library(dplyr)
library(tidyr)
library(ggplot2)

###############################################################################
# 1. Get new titles
###############################################################################

# NewsApi token: 
token <- "f4a88e83555a4ffd91669835d38efedf"

# End points
top_headlines <- "https://newsapi.org/v2/top-headlines"
everything <- "https://newsapi.org/v2/everything"

# Parameters
keyword <- " "
categories <- c("general","business", "entertainment","health","science",
                "sports","technology")
# Result per page
pageSize <- 100

# Current page retrieved
page <- 1

country <- 'ar'
lang <- "es"
sortBy <- c("publishedAt","relevancy","popularity")
from <- 2019-01-01
to <- 2019-06-06

req_top <- api(top_headlines) %>%
  api_query_(country = country, 
            apiKey=token, 
            category=categories[1], 
            pageSize=pageSize,
            language=lang,
            #=keyword,
            wt="json") 

req_all <- api(everything) %>%
  api_query_(apiKey = token, 
            q = "kirchner", 
            language = lang,
            sortBy = sortBy[1],
            pageSize = pageSize,
            page = page,
            from = from,
            to = to,
            wt ="json") 

json_top <- toJSON(req_top$articles, 
                   Date="ISO8601", POSIXt = "ISO8601", null="null", na="null")
prettify(json_top)

df_top <- fromJSON(json_top, simplifyDataFrame = T, flatten = T, simplifyVector = T)

# Format news API
df_top$author <- NULL
df_top$title <- unlist(df_top$title)
df_top$description <- unlist(df_top$description)
df_top$url <- unlist(df_top$url)
df_top$urlToImage <- unlist(df_top$urlToImage)
df_top$publishedAt <- unlist(as.Date(df_top$publishedAt))
df_top$content <- unlist(df_top$content)
df_top$source.id <- NULL
df_top$source.name <- unlist(df_top$source.name)

summary(df_top)
df_top <- df_top %>%
  filter(!(source.name %in% c("La Nacion", "Perfil.com", "Clarin.com")))

#
# 2. Sentiment Analisys
#
for (i in 1:nrow(df_top)) {
  my_text <- df_top$title[i]
  char_v <- get_sentences(my_text)
  
  if(i == 1){
    nrc_data <- get_nrc_sentiment(char_v, language="spanish")
    nrc_sum <- colSums(nrc_data)
    print("piu")
  } else {
    aux_data <- get_nrc_sentiment(char_v, language="spanish")
    nrc_sum <- nrc_sum + colSums(aux_data)
    }
}


barplot(
  prop.table(nrc_sum[1:8]), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1,
  col= rainbow(8),
  main = "Emotions in Sample text", xlab="Percentage"
)

