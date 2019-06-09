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


# #################################################################################################
# NewsApi token: f4a88e83555a4ffd91669835d38efedf
# #################################################################################################

#install.packages("request")
library(request)

url = "https://newsapi.org/v2/top-headlines?country=ar&apiKey=f4a88e83555a4ffd91669835d38efedf"

json <- api("https://newsapi.org/v2/top-headlines") %>%
      api_query(country = ar, apiKey=f4a88e83555a4ffd91669835d38efedf, 
                category=general, pageSize=100, 
                q=cristina, wt="json") 

json <- api("https://newsapi.org/v2/everything") %>%
      api_query(apiKey=f4a88e83555a4ffd91669835d38efedf, 
                q=kirchner, language=es,wt="json",pageSize=100) 

json <- toJSON(json)
df <- fromJSON(json)

prettify(json)

title <- unlist(df$articles$title)
source <- unlist(df$articles$source)
date <- unlist(df$articles$publishedAt)

all <- cbind(title,source[1:100],date)
write.csv2(all,"caca.csv")


## sentiment analisys
#install.packages("NLP")
library(NLP)

#install.packages("syuzhet")
library(syuzhet)

my_example_text <- "I begin this story with a neutral statement.  
  Basically this is a very silly test.  
  You are testing the Syuzhet package using short, inane sentences.  
  I am actually very happy today. 
  I have finally finished writing this package.  
  Tomorrow I will be very sad. 
  I won't have anything left to do. 
  I might get angry and decide to do something horrible.  
  I might destroy the entire package and start from scratch.  
  Then again, I might find it satisfying to have completed my first R package. 
  Honestly this use of the Fourier transformation is really quite elegant.  
  You might even say it's beautiful!"
s_v <- get_sentences(my_example_text)
s_t <- get_tokens(s_v)
sent <- get_sentiment(s_t)

plot(
  sent, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

##
path_to_a_text_file <- system.file("extdata", "quijote.txt",package = "syuzhet")
my_text <- get_text_as_string(path_to_a_text_file)
char_v <- get_sentences(my_text)
method <- "nrc"
lang <- "spanish"
my_text_values <- get_nrc_sentiment(char_v, language=lang)
head(my_text_values,10)




