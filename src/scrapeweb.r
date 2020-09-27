#Hello friend
print("Starting research...")

#install.packages("selectr")
#install.packages("xml2")
#install.packages("rvest")
#install.packages("jsonlite")


library(jsonlite)
library(selectr)
library(XML)
library(xml2)
library(stringr)
library(rvest)

#---- Viewing pictures
#install.packages("magick")
# library(magick)
# pic <- df_top$urlToImage[[1]]
# myPic <- image_read(pic, strip=T)
# scale to 300px
# myPic <- image_scale(myPic, 300)
# print(myPic)
#----

#--- comparing words
#install.packages("SciencePo", repos="http://R-Forge.R-project.org")
#library(SciencePo)
#---

#----- check spelling
# library(hunspell)
# dictionary("es_ES")
# 
# bad <- hunspell(df_top$description[31], dic="es_ES")
# print(bad[[1]])
# 
# hunspell_suggest(bad[[1]])
#-----

# scrapear IMDB
a_posteriori <- "https://www.imdb.com/search/title/?title=batman"

scrapeNode = function(webpage, node){
  node_html <- html_nodes(webpage, paste("h1#",title,sep=""))
  text <- html_text(title_html)
  
  # remove all space and new lines
  text <- str_replace_all(text, "[\r\n]", "")
  text <- str_trim(text)
  return(text)
}

s1 <- "https://newsapi.org/v2/top-headlines?sources=google-news&apiKey=API_KEY"
s2 <- "https://www.google.com/search?q=argentina+cristina&source=lnms&tbm=nws&sa=X&ved=0ahUKEwjrj5X9psziAhVeILkGHcjnAEgQ_AUIESgC&biw=1288&bih=791"


###################################################################################################
# SCRAPPING TUTORIAL FROM AMAZON
# https://medium.com/free-code-camp/an-introduction-to-web-scraping-using-r-40284110c848
###################################################################################################

#Specifying the url for desired website to be scrapped
url <- "https://www.amazon.in/dp/B07J2NZDP5/ref=dp_prsubs_3"
#Reading the html content from Amazon
webpage <- read_html(url)
summary(webpage)

#scrape title of the product
title_html <- html_nodes(webpage, "h1#title")
title <- html_text(title_html)

# remove all space and new lines
title <- str_replace_all(title, "[\r\n]" , "")
title <- str_trim(title)

#scrapeNode(webpage, "title") not working

head(title)

# scrape the price of the product
price_html <- html_nodes(webpage, "span#priceblock_ourprice")
price <- html_text(price_html)
# remove all space and new lines
price <- str_replace_all(price, "[\r\n]" , "")
price <- str_trim(price)

# print price value
head(price)

# scrape product description
desc_html <- html_nodes(webpage, "div#renewedProgramDescriptionAtf")
desc <- html_text(desc_html)
# replace new lines and spaces
desc <- str_replace_all(desc, "[\r\n\t]" , "")
desc <- str_trim(desc)
head(desc)

desc2_html <- html_nodes(webpage, "div#featurebullets_feature_div")
desc2 <- html_text(desc2_html)
# replace new lines and spaces
desc2 <- str_replace_all(desc2, "[\r\n\t]" , "")
desc2 <- str_trim(desc2)
head(desc2)


# scrape product rating 
rate_html <- html_nodes(webpage, "span#acrPopover")
rate <- html_text(rate_html)
# remove spaces and newlines and tabs 
rate <- str_replace_all(rate, "[\r\n]" , "")
rate <- str_trim(rate)
# print rating of the product
head(rate)

# Scrape size of the product
size_html <- html_nodes(webpage, "div#variation_size_name")
size_html <- html_nodes(size_html, "span.selection")
size <- html_text(size_html)
# remove tab from text
size <- str_trim(size)
# Print product size
head(size)

# Scrape product color
color_html <- html_nodes(webpage, "div#variation_color_name")
color_html <- html_nodes(color_html, "span.selection")
color <- html_text(color_html)
# remove tabs from text
color <- str_trim(color)
# print product color
head(color)

#Combining all the lists to form a data frame
product_data <- data.frame(Title = title, Price = price,
                           Description = desc, Rating = rate, 
                           Size = size, Color = color)
#Structure of the data frame
str(product_data)

# convert dataframe into JSON format
json_data <- toJSON(product_data)

# print output
prettify(json_data)