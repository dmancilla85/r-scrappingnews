
source("./src/framework.r")

# Sentiment analisys
#library(NLP)
library(syuzhet)


###############################################################################
# 1. Get newest titles
###############################################################################
newsApi <- NewsApi(
  token = "f4a88e83555a4ffd91669835d38efedf",
  # NewsApi token
  pageSize = 100,
  # Result per page
  country = 'ar',
  # Country
  language = 'es',
  # Language
  from = paste('"', '2019-11-17', '"', sep = ""),
  # older as one month back (free User)
  to = paste('"', '2019-11-19', '"', sep = ""),
  # dates surround with "
  query = paste('"', 'Añez', '"', sep = ''),
  # query surround with "
  searchInTitles = TRUE,
  # Search in titles
  category = "general",
  # category for top headlines
  sortBy = "publishedAt",
  # sort criteria
  onlyLatestNews = TRUE
)


#req_top <- getNews(newsApi)
#newsApi$topNews <- FALSE

df_req <- getNews(newsApi)


###############################################################################
# 3. Sentiment Analisys
###############################################################################

df_top <-df_req #%>% filter(source.name %in% c("La Nacion", "Clarin.com", "Perfil.com","CNN"))


df_top$id <- (1:nrow(df_top))

# Collect all descriptions
for (i in 1:nrow(df_top)) {
  my_text <- df_top$content[i]
  char_v <- get_sentences(my_text)
  
  if (i == 1) {
    nrc_data <-
      get_nrc_sentiment(char_v,
                        language = "spanish")
    nrc_sum <- colSums(nrc_data)
    nrc_data$id <- i
  } else {
    aux_data <-
      get_nrc_sentiment(char_v,
                        language = "spanish")
    #print(aux_data)
    nrc_sum <- nrc_sum + colSums(aux_data)
    auxdata <- cbind(df_top[i,]$id)
    aux_data$id <- i
    nrc_data <- rbind(nrc_data, aux_data)
  }
}

df <- df_top %>% inner_join(nrc_data, by="id") #%>% filter(id==3)

###########################
# tb <- data.frame(rbind(nrc_sum[1:7]))
# colnames(tb) <-
#   c("Ira",
#     "Esperanza",
#     "Asco",
#     "Miedo",
#     "Alegría",
#     "Tristeza",
#     "Sorpresa")
df <- pivot_longer(df, cols = c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive"), names_to = "Sentimiento") %>% filter(!(Sentimiento %in% c("positive","negative","trust")))



df %>% ggplot(aes(x = Sentimiento, y = value, fill = source.name)) +
  geom_bar(stat = "identity",col="black") + ggtitle(newsApi$query, subtitle = nrow(df_top)) +
  coord_flip()

