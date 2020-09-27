
# Sentiment Analysis
if(!require(syuzhet)){
  install.packages("syuzhet")
  library(syuzhet)
}

# String manipulations
if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}

# Handling promises
if (!require(future)) {
  install.packages("future")
  library(future)
}

# TreeMap
if (!require(treemap)) {
  install.packages("treemap")
  library(treemap)
}

# TreeMap
if (!require(treemapify)) {
  install.packages("treemapify")
  library(treemapify)
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
if (!require(request)) {
  instal.packages("request")
  library(request)
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
    category = "character"
  ),
  methods = list(
    initialize = function(p_pageSize = 100,
                          p_country = "ar",
                          p_language = "es",
                          p_sortBy = "publishedAt",
                          p_from = "",
                          p_to = "",
                          p_query = "",
                          p_searchInTitles = FALSE,
                          p_topNews = FALSE,
                          p_category = "general") {
      .self$token <- "f4a88e83555a4ffd91669835d38efedf"
      .self$pageSize <- p_pageSize
      .self$country <- p_country
      .self$language <- p_language
      .self$sortBy <- p_sortBy
      .self$from <- paste('"', p_query, '"', sep = "")
      .self$to <- paste('"', p_to, '"', sep = "")
      .self$query <- paste('"', p_query, '"', sep = "")
      .self$searchInTitles <- p_searchInTitles
      .self$topNews <- p_topNews
      .self$category <- p_category
    }
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


# ### FUNCTIONS ### #

# ################################# #

# ################################# #
processWithNRC <- function(df, lang = "es") {
  df$id <- (1:nrow(df))
  
  language <- "spanish"
  
  if (lang == "es") {
    language <- "spanish"
  } else if (lang == "en") {
    language <- "english"
  }
  
  
  # Collect all descriptions
  for (i in 1:nrow(df)) {
    my_text <- df$content[i]
    char_v <- get_sentences(my_text)
    
    if (i == 1) {
      nrc_data <- get_nrc_sentiment(char_v, language = "spanish")
      nrc_sum <- colSums(nrc_data)
      nrc_sum <- append(nrc_sum, i)
      names(nrc_sum) <- c( "anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive","id");
    } else {
      aux_data <- get_nrc_sentiment(char_v, language = "spanish")
      aux_sum <- colSums(aux_data)
      aux_sum <- append(aux_sum, i)
      names(aux_sum) <- c( "anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive","id");
      
      nrc_sum <- rbind(nrc_sum, aux_sum)
    }
  }
  
  df <- df %>% inner_join(as.data.frame(nrc_sum), by = "id")
  
  return(df)
}


# ################################# #

# ################################# #
plotSentiment <- function(df, title, subtitle) {
  nrc  <-
    pivot_longer(
      df,
      cols = c(
        "anger",
        "anticipation",
        "disgust",
        "fear",
        "joy",
        "sadness",
        "surprise",
        "trust",
        "negative",
        "positive"
      ),
      names_to = "Sentimiento"
    ) %>% filter(!(Sentimiento %in% c("positive", "negative", "trust")))
  nrc
  nrc[nrc$Sentimiento == "anger", ]$Sentimiento = "Ira"
  nrc[nrc$Sentimiento == "anticipation", ]$Sentimiento = "Expectativa"
  nrc[nrc$Sentimiento == "disgust", ]$Sentimiento = "Desagrado"
  nrc[nrc$Sentimiento == "fear", ]$Sentimiento = "Miedo"
  nrc[nrc$Sentimiento == "joy", ]$Sentimiento = "Alegría"
  nrc[nrc$Sentimiento == "sadness", ]$Sentimiento = "Tristeza"
  nrc[nrc$Sentimiento == "surprise", ]$Sentimiento = "Sorpresa"
  
  plot <- nrc %>% ggplot(aes(x = Sentimiento, y = value, fill = source.name)) +
    geom_bar(stat = "identity") + 
    ggtitle(title, subtitle) +
    coord_flip() + 
    xlab("Sentimiento") + 
    ylab("Puntaje") + 
    labs(fill="Fuente") + 
    scale_color_brewer() +
    theme_dark() +
    theme_light()
  
  return(plot)
  
}


# ################################# #

# ################################# #
getNews <- function(obj) {
  if (isTRUE(obj$topNews)) {
    df <- getTopHeadLines(obj)
    return(df)
  } else {
    df <- getEverything(obj)
    return(df)
  }
}


# ################################# #

# ################################# #
showError <- function(err, type = "Error:") {
  print(paste(type, err$message))
  print(paste("Call:", err$call))
}


# ################################# #

# ################################# #
verifyResponse <- function(myRequest) {
  df <- data.frame(
    title = character(),
    description = character(),
    url = character(),
    urlToImage = character(),
    publishedAt = as.Date(character()),
    content = character(),
    source.name = character()
  )
  
  tryCatch(
    if (myRequest$status != "ok") {
      print(paste(myRequest$status, myRequest$code, ":", myRequest$message))
    } else {
      df <- convertFromJSON(myRequest)
    },
    warning = function(err) {
      showError(err, "Warning")
    },
    error = function(err) {
      showError(err)
    }
  )
  
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
  
  totalPages <- as.integer(df_request$totalResults[1] / obj$pageSize)
  
  if (totalPages == 0) {
    print("No hay resultados")
    return(df_request)
    
  }
  
  if (totalPages == 1) {
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
            #q= obj$query Not available for now
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
            #q= obj$query Not available for now
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
  df_request$description <- unlist(df_request$description) #
  
  df_request[df_request$description  %in% c("", "NULL"), ]$description <-
    df_request[df_request$description  %in% c("", "NULL"), ]$title
  
  df_request$url <- as.character(df_request$url)
  df_request$url <- unlist(df_request$url) #ok
  
  
  df_request$urlToImage <- as.character(df_request$urlToImage)
  df_request$urlToImage <- unlist(df_request$urlToImage) #ok
  
  df_request$publishedAt <- as.character(df_request$publishedAt)
  df_request$publishedAt <- unlist(df_request$publishedAt) #ok
  df_request$publishedAt <- as.Date(df_request$publishedAt)
  
  df_request$content <- as.character(df_request$content)
  df_request$content <- unlist(df_request$content)
  df_request$content <-
    str_replace_all(df_request$content, "chars]", "")
  df_request$content <-
    str_remove_all(df_request$content, "[+,0-9]")
  df_request$content <- gsub("\\[|\\]", "", df_request$content)
  
  df_request[df_request$content %in% c("", "NULL"), ]$content <-
    df_request[df_request$content %in% c("", "NULL"), ]$description
  
  df_request$source.name <- as.character(df_request$source.name)
  df_request$source.name <- unlist(df_request$source.name)
  
  return(df_request)
}