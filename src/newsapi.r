
source("./src/framework.r")

###############################################################################
# 1. Get newest titles
###############################################################################
myQuery <- "bolivia"

newsApi <- NewsApi(
  # Language
  p_from = '2019-11-17',
  # older as one month back (free User)
  p_to = '2019-11-19',
  # dates surround with "
  p_query = myQuery,
  # sort criteria
  p_topNews = TRUE
)

df_req <- getNews(newsApi)

View(df_req)

###############################################################################
# 3. Sentiment Analisys
###############################################################################

df_top <-
  df_req #%>% filter(source.name %in% c("La Nacion", "Clarin.com", "Perfil.com","CNN"))

nrc <- processWithNRC(df_top)

View(nrc)

plotSentiment(nrc,myQuery,"")
