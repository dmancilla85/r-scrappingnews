


# Handling promises
if (!require(future)) {
  install.packages("future")
  library(future)
}

# Data  manipulations
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}

# Data visualizations
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# JSON format
if (!require(jsonlite)) {
  install.packages("jsonlite")
  library(jsonlite)
}

# API connections
if (!require(future)) {
  instal.packages("future")
  library(future)
}


# Variables
NewsApi <- setRefClass(
  "NewsAPI",
  fields = list(
    token = "character",
    pageSize = "numeric",
    country = "character",
    language = "character",
    sortBy = "character",
    from = "character",
    to = "character",
    query = "character",
    searchInTitles = "logical",
    topNews = "logical",
    category ="character"
  )
)

# Sort criteria
newsApiSortBy <- c("publishedAt", "relevancy", "popularity")

newsApiCategories <-
  c("general",
    "business",
    "entertainment",
    "health",
    "science",
    "sports",
    "technology")



# Functions
getNews <- function(obj){
  if(isTRUE(obj$topNews)){
    df <- getTopHeadLines(obj)
    return(df)
  } else {
    df <- getEverything(obj)
    return(df)
  }
}

# ################################# #

# ################################# #
verifyResponse <- function(request) {
  df <- data.frame(
    title = character(),
    description = character(),
    url = character(),
    urlToImage = character(),
    publishedAt = as.Date(character()),
    content = character(),
    source.name = character()
  )
  
  if (request$status != "ok") {
    print(paste(request$status, request$code, ":", request$message))
  } else {
    df <- convertFromJSON(request)
  }
  
  return(df)
}


# ################################# #

# ################################# #
getEverything <- function(obj, 
                          freeAccountMode = TRUE) {
  df_request = getEverythingPerPage(obj, 1)
  
  if (nrow(df_request) == 0 || isTRUE(freeAccountMode)) {
    return(df_request)
  }
  
  df_request$page <- 1
  
  
  totalPages <- as.integer(df_request$totalResults[1] / pageSize)
  print(totalPages)
  
  if (totalPages == 0) {
    print("No hay resultados")
    return(df_request)
  }
  
  if(totalPages == 1){
    df_request$totalResults <- NULL
    return(df_request)
  }
  
  for (i in seq(2, totalPages, by = 1)) {
    #print(paste("Retrieving page", i, "..."))
    #Sys.sleep(0.5)
    
    aux <-
      getEverythingPerPage(obj, i)
    
    if (nrow(aux) == 0) {
      df_request$totalResults <- NULL
      return (df_request)
    }
    
    # Unir todos los resultados
    aux$page <- i
    df_request <- rbind(df_request, aux)
    
  }
  
  # Esta variable ya no la necesito
  df_request$totalResults <- NULL
  
  return(df_request)
}


# ################################# #
# Query all articles availables in NewsAPI
# between a range of dates
# ################################# #
getEverythingPerPage <- function(obj,
                                 currentPage = 1) {
  
  ep_everything <- "https://newsapi.org/v2/everything"
  
  
  if (isTRUE(obj$searchInTitles)) {
    promise <- future (
      api(ep_everything) %>%
        api_query_(
          apiKey = obj$token,
          qInTitle = obj$query,
          language = obj$language,
          sortBy = obj$sortBy,
          pageSize = obj$pageSize,
          page = currentPage,
          from = obj$from,
          to = obj$to,
          wt = "json"
        ) %>% api_error_handler(warn_for_status)
    ) %plan% multiprocess
    
  } else {
    promise <- future (
      api(ep_everything) %>%
        api_query_(
          apiKey = obj$token,
          q = obj$query,
          language = obj$language,
          sortBy = obj$sortBy,
          pageSize = obj$pageSize,
          page = currentPage,
          from = obj$from,
          to = obj$to,
          wt = "json"
        ) %>% api_error_handler(warn_for_status)
    ) %plan% multiprocess
  }
  
  request <- value(promise)
  
  df_request <- verifyResponse(request)
  
  if (length(df_request) > 0) {
    df_request$totalResults <- request$totalResults
  }
  
  return(df_request)
}

# ################################# #
# Retrieve newest headlines from News API
# ################################# #
getTopHeadLines <-
  function(obj) {
    ep_top_headlines <- "https://newsapi.org/v2/top-headlines"
    
    
    if (is.null(obj$category) || obj$category == "") {
      promise <- future(
        api(ep_top_headlines) %>%
          api_query_(
            country = obj$country,
            apiKey = obj$token,
            pageSize = obj$pageSize,
            language = obj$language,
            wt = "json"
          ) %>% api_error_handler(warn_for_status)
      ) %plan% multiprocess
      
    } else {
      promise <- future(
        api(ep_top_headlines) %>%
          api_query_(
            country = obj$country,
            apiKey = obj$token,
            category = obj$category,
            pageSize = obj$pageSize,
            language = obj$language,
            wt = "json"
          ) %>% api_error_handler(warn_for_status)
      ) %plan% multiprocess
      
    }
    
    request <- value(promise)
    
    df_request <- verifyResponse(request)
    
    return(df_request)
  }


# ################################# #

# ################################# #
convertFromJSON <- function(request) {
  json_top <- toJSON(
    request$articles,
    Date = "ISO8601",
    POSIXt = "ISO8601",
    null = "null",
    na = "null"
  )
  
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
  
  df_request$publishedAt <- as.character(df_request$publishedAt)
  df_request$publishedAt <- unlist(df_request$publishedAt) #ok
  df_request$publishedAt <- as.Date(df_request$publishedAt)
  
  df_request$content <- as.character(df_request$content)
  df_request$content <- unlist(df_request$content)
  
  df_request$source.name <- as.character(df_request$source.name)
  df_request$source.name <- unlist(df_request$source.name)
  
  return(df_request)
}