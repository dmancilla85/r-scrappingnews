

#install.packages("future")
library(future)
library(utils)

# Common libraries
library(dplyr)
library(tidyr)
library(ggplot2)
# JSON
library(jsonlite)
# Call API service
library(request)

# Sentiment analisys
#library(NLP)
library(syuzhet)

# Retrieve newest headlines from News API
getTopHeadLines <-
  function(token,
           country = 'ar',
           language = 'es',
           category = NULL,
           pageSize = 50) {
    ep_top_headlines <- "https://newsapi.org/v2/top-headlines"
    
    
    if (is.null(category)) {
      tryCatch(promise <- future(api(ep_top_headlines) %>%
        api_query_(
          country = country,
          apiKey = token,
          pageSize = pageSize,
          language = language,
          wt = "json"
        )) %plan% multiprocess,
        warning=function(err){print(err$message)},
        error=function(err){print(err$message)})
      
    } else {
      promise <- future( api(ep_top_headlines) %>%
        api_query_(
          country = country,
          apiKey = token,
          category = category,
          pageSize = pageSize,
          language = language,
          wt = "json"
        )) %plan% multiprocess
      
    }
    
    tryCatch(request <- value(promise),
             warning=function(err){print(err$message)},
             error=function(err){print(err$message)})
    
    if (request$status != "ok") {
      return(NULL)
      
      print("Can't get response")
    }
    
    request_df <- convertFromJSON(request)
    
    return(request_df)
  }


# Query all articles availables in NewsAPI between a range of dates
getEverythingPerPage <- function(token,
                                 query,
                                 from,
                                 to,
                                 language = 'es',
                                 sortBy = "relevancy",
                                 pageSize = 20,
                                 currentPage = 1) {
  ep_everything <- "https://newsapi.org/v2/everything"
  
  promise <- future (
    api(ep_everything) %>%
    api_query_(
      apiKey = token,
      q = query,
      language = language,
      sortBy = sortBy,
      pageSize = pageSize,
      page = currentPage,
      from = from,
      to = to,
      wt = "json"
    )) %plan% multiprocess
  
  request <- value(promise)
  
  if (request$status != "ok") {
    return(NULL)
    
    print("Can't get response")
  }
  
  request_df <- convertFromJSON(request)
  request_df$totalResults <- request$totalResults
  
  return(request_df)
}

getEverything <- function(token,
                          query,
                          from,
                          to,
                          language = 'es',
                          sortBy = 'relevancy',
                          pageSize = 20) {
  request_df = getEverythingPerPage(token, query, from, to, language, sortBy, pageSize, 1)
  request_df$page <- 1
  
  
  totalPages <- as.integer(request_df$totalResults[1] / pageSize)
  print(totalPages)
  
  print("comienzo a iterar")
  
  for (i in seq(2, totalPages, by = 1)) {
    print(paste("pagina ", i))
    #Sys.sleep(3)
    
    tryCatch(aux <- getEverythingPerPage(token, query, from, to, language, sortBy, pageSize, i),
             warning=function(e){print(summary(e))},
             error=function(e){print(e$message)})
    
    # Unir todos los resultados
    aux$page <- i
    print("a unir...")
    request_df <- rbind(request_df, aux)
    
  }
  
  
  
  return(request_df)
}

convertFromJSON <- function(request) {
  json_top <- toJSON(
    request$articles,
    Date = "ISO8601",
    POSIXt = "ISO8601",
    null = "null",
    na = "null"
  )
  #print(prettify(json_top))
  
  df_request <-
    fromJSON(
      json_top,
      simplifyDataFrame = T,
      flatten = T,
      simplifyVector = T
    )
  
  # Cleaning data and format news API
  df_request$author <- NULL
  df_request$source.id <- NULL
  
  df_request$title <- unlist(df_request$title) #ok
  
  df_request$description <- as.character(df_request$description)
  df_request$description <- unlist(df_request$description) #ok
  
  df_request$url <- as.character(df_request$url)
  df_request$url <- unlist(df_request$url) #ok
  
  
  df_request$urlToImage <- as.character(df_request$urlToImage)
  df_request$urlToImage <- unlist(df_request$urlToImage) #ok
  
  df_request$publishedAt <-
    as.Date(unlist(df_request$publishedAt)) #ok
  
  df_request$content <- as.character(df_request$content)
  df_request$content <- unlist(df_request$content)
  
  df_request$source.name <- as.character(df_request$source.name)
  df_request$source.name <- unlist(df_request$source.name)
  
  #glimpse(df_top)
  #df_top %>% filter((source.name %in% c("La Nacion", "Perfil.com", "Clarin.com")))
  return(df_request)
}


###############################################################################
# 1. Get newest titles
###############################################################################
# NewsApi token:
token <- "f4a88e83555a4ffd91669835d38efedf"

# Parameters
keyword <- " "
categories <-
  c("general",
    "business",
    "entertainment",
    "health",
    "science",
    "sports",
    "technology")
# Result per page
pageSize <- 100

# Current page retrieved
page <- 1

# Country
country <- 'ar'

# Language
lang <- "es"

# Sort criteria
sortBy <- c("publishedAt", "relevancy", "popularity")

# Date range
from <- as.POSIXct("2019-11-10")
to <- as.POSIXct("2019-11-16")

# Query word
query <- paste('"','Morales','"', sep='')

req_top <- getTopHeadLines(token, country, lang)
req_all <- getEverything(token, query, from, to, pageSize = 50)

test <- api("https://newsapi.org/v2/everything") %>%
  api_query_(
    apiKey = token,
    q = query,
    language = 'es',
    sortBy = sortBy,
    pageSize = pageSize,
    page = 1,
    from = as.integer(from),
    to = as.integer(to),
    wt = "json"
  )

###############################################################################
# 3. Sentiment Analisys
###############################################################################

# Collect all descriptions
for (i in 1:nrow(df_top)) {
  my_text <- df_top$title[i]
  char_v <- get_sentences(my_text)
  
  if (i == 1) {
    nrc_data <- get_nrc_sentiment(char_v, language = "spanish")
    nrc_sum <- colSums(nrc_data)
    print("piu")
  } else {
    aux_data <- get_nrc_sentiment(char_v, language = "spanish")
    nrc_sum <- nrc_sum + colSums(aux_data)
  }
}


barplot(
  prop.table(nrc_sum[1:8]),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  col = rainbow(8),
  main = "Emotions in Sample text",
  xlab = "Percentage"
)
nrc_data
