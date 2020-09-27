
source("./src/framework.r")

###############################################################################
# 1. Get newest titles
###############################################################################
myQuery <- "Covid-19"

newsApi <- NewsApi(
  # Language
  p_from = '2020-03-01',
  # older as one month back (free User)
  p_to = '2020-03-19',
  # dates surround with "
  p_query = myQuery,
  # sort criteria
  p_topNews = TRUE
)

df_req <- getNews(newsApi)

View(df_req)
df_req$content[1]

###############################################################################
# 3. Sentiment Analisys
###############################################################################

df_top <-
  df_req #%>% filter(source.name %in% c("La Nacion", "Clarin.com", "Perfil.com","CNN"))

nrc <- processWithNRC(df_top)
nrc %>% filter(anger>0)  %>% select(title,anger) 
head(nrc)

plotSentiment(nrc,myQuery,"")

otro <- nrc %>% select(anger,anticipation,disgust,fear,joy,sadness,surprise)
elotro <- colSums(otro)
elotro <- as.data.frame(elotro)
elotro
rownames(elotro) <- c("Ira","Expectativa","Rechazo", "Miedo","Alegría","Tristeza","Sorpresa")
elotro$sentimiento <- rownames(elotro)
names(elotro) <- c("valor","sentimiento")
elotro
elotro %>% 
  ggplot(aes(area=valor,fill=valor,label=sentimiento)) + geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE) + ggtitle("Lexicones NRC") + theme_dark()
