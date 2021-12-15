library(readr)
library(dplyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(tidyverse)
library(na.tools)
library(janitor)
library(devtools)
install_github('arilamstein/choroplethrZip@v1.4.0')

install.packages("extracat")

library(choroplethrZip)
library(GGally)
library(lubridate)
library(zoo)
library(scales)
library(ggmap)
library(scales)
library(stringr)
library(zipcodeR)
library(leaflet)
library(extracat)
library(gridExtra)
library(sentimentr)
library(ratios)


#### Some code has been copied from "author: Ankit Peshin, Sarang Gupta, Ankita Agrawal"

options(scipen = 999)
listings <- read_csv("./Data/listings_1.csv")
reviews <- read_csv("./Data/reviews.csv")
calendar <- read_csv("./Data/calendar.csv")
neighborhoods <- read_csv("./Data/neighborhoods.csv")

## Description of the Data
# listings = name, host, host name, neighborhood, neighborhood group, latitutde, longitutde, room type, price, minimum # of nights, number of reviews, last review, reviews per month, host listing number, availability, number of reviews, license 
# reviews - listing id, id , date, reviewer id, reviewer name, comments
# calendar - details about booking for the next year about listing, date, available, price, minimum nights

# 1836 unique listings are provided for the washington DC area 
# over 30,000 reviews have been left from November 2010-December 2021


# The main topics of data that I will be analyzing 
# Demand and Price Analysis 
# User Review
# other interesting things
# possible data analysis 


#First need to clean the Data 

## Dealing with data with misssing values 

listingsdf1 <-read_csv("./Data/listings_1.csv")
missingdata <- listingsdf1 
missingdata[missingdata == ""] <- NA


### Spatial Analysis 
### This section is looking at the various location of Airbnbs in DC

listingdf <- read.csv('./Data/listings_1.csv')

leaflet(listingdf) %>%
  addTiles() %>%
  addMarkers(~longitude, ~latitude,labelOptions = labelOptions(noHide = F),clusterOptions = markerClusterOptions(),popup = paste0("<b> Name: </b>", listingdf$name , "<br/><b> Host Name: </b>", listingdf$host_name, "<br> <b> Price: </b>", listingdf$price, "<br/><b> Room Type: </b>", listingdf$room_type, "<br/><b> Property Type: </b>", listingdf$property_type
  )) %>% 
  setView(-77.00, 38.9, zoom = 11.5) %>%
  addProviderTiles("CartoDB.Positron")


# This is a interactive map of all the listins within DC. It is in a clustered form until you zoom in. Each point you can select and click on a location to see Name, Host Name, Price, Room Type, Propery type. 


####Looking at the relationship between property type and neighbourhood. Room vs. Entire Apt/House
# NOT WORKING 
propertydf <- listingdf %>%
  group_by(host_neighbourhood, room_type) %>%
  summarise(Freq = n(), .groups = "drop")
propertydf <- propertydf %>%
  filter(room_type %in% c("Entire home/apt", "Private room")) %>%
  group_by(host_neighbourhood) %>%
  summarise(sum = n())

totalproperty <- listingdf %>%
  filter(room_type %in% c("Entire home/apt", "Private room")) %>%
  group_by(host_neighbourhood) %>%
  summarise(sum = n())

propertyratio <- merge(propertydf, totalproperty, by = "host_neighbourhood" )
propertyratio <- propertyratio %>% mutate(ratio = Freq/sum )


### Demand and Price Analysis
# Look at the demand over the years since the beginning Airbnbs in the DC area
# look at the relationship of price vs demand = do prices of listings flucutate with demand, how do prices vary by days of the week
# To find the demand, will use number of reviews as the indicator for demand

# need to change dates in reviewsNUM to just be the year, so then the dates are confused the bottom ticks can be made into just years 

#### How popular has Airbnb become in Washington DC?

reviewsNUM <- reviews1 %>% 
  group_by(date = reviews1$date) %>%
  summarise(number= n())

reviewsNUM <- reviewsNUM[order(as.Date(reviewsNUM[["date"]], format ="%m/%d/%Y")),]


ggplot(reviewsNUM, aes(date,number))+
  geom_point(na.rm = TRUE, color = "blue", alpha=0.5) +
  geom_smooth(color= "red") +
  ggtitle("How popular is Airbnb?",
          subtitle = "Number of Reviews Across Years") +
  labs(x= "Year", y= "Unique Listings Recieving Reviews") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey")) +
  theme(plot.caption = element_text(color = "grey")) 

## NEED TO FIX GRAPH WITH YEARS AND ANGLES 


## 

       
##### How is Airbnb priced across the year? 

## We wanted to see if the pricing of the postings followed a similar trend after seeing the pattern in demand. 
#### To address the aforementioned issue, we used the data from the 'calendar' table to look at the daily 
#### average prices of the listings through time.

calendar$price <- as.numeric(gsub(",","",substring(calendar$price, 2)))

calendarAll <- calendar %>%
  group_by(date = date) %>%
  summarise(averagePrice = mean(price, na.rm = TRUE )) %>%
  mutate(year = year(date), commonYear = paste("2021", substring(date,6), sep = "-"))

calendarAll$year  <- as.factor(as.character(calendarAll$year))
calendarAll$commonYear <- ymd(calendarAll$commonYear)


ggplot(calendarAll, aes(commonYear, averagePrice)) +
  geom_point(na.rm = TRUE, alpha=0.5, color  = "Blue") +
  geom_smooth(color= "Orange") +
  facet_grid(~year) +
  ggtitle("Seasonality In Price",
          subtitle = "Average listing price across months") +
  labs(x= "Month", y= "Average price acorss Listings") +
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold", color = "grey68"),
        plot.caption = element_text(color = "grey68") + scale_x_date(labels = date_format("%b")))

       
##### As the year advances, the average price of all listings tends to rise, peaking in December. Except in November and December, when the number of reviews (an indicator of demand) begins to fall, the pattern is identical to that of the number of reviews/demand. This appears to be counter-intuitive, as one would anticipate the price to fall as demand falls. This could be due to our assumption that the quantity of reviews reflects demand, which isn't always the case.

####On the above graphs, we can also notice two sets of points indicating that average prices on certain days were greater than on other days. To further comprehend this phenomena, we'll create a box plot showing average costs by weekday.
      
calendarAll <- calendarAll %>%
  mutate(day = strftime(date, '%A'))
calendarAll$day <- factor(calendarAll$day, levels = c("Sunday","Monday", "Tuesday", 
                                                      "Wednesday", "Thursday", "Friday", "Saturday"), 
                          labels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                                   "Friday", "Saturday"))

ggplot(calendarAll, aes(x=factor(day),
                        y = averagePrice)) +
  geom_boxplot(fill = "#FF5A5F", color = "#565A5C") +
  geom_jitter(alpha = 0.05, width = 0.1, color = "#007A87") +
  ggtitle("Is it expensive to travel on weekends?",
          subtitle = "Boxplots of Price by Day of the Week") +
  labs(x = "Day of the week", y = "Average Price") + 
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold", color = "grey35"),
        plot.caption = element_text(color = "grey68"))

We can see that Fridays and Saturdays have a higher concentrated price for the renting on the weekends. 

### Occupancy Rate by Month 

### I'll end this section's examination by looking at the occupancy forecast for the coming year. 
#### We will use the table 'calendar' to determine the % occupancy for the next year, i.e., what
### proportion of appartments have already been booked as of November 3, 2018 (the day the data 
### was obtained). We were unable to get historical occupancy data and, as a result, were unable
### to investigate what the real occupancy rates are.


#### THIS SECTION NOT WORKING STILL NEED TO DOWNLOAD makeR
calendar <- calendar %>%
  mutate(booked = ifelse(available=="f", 1, 0))
groupedcalnedar <- calendar %>%
  group_by(date = date) %>%
  summarise(totalBooked = sum(booked, na.rm = TRUE), totalListings = n()) %>%
  mutate(percent_booked =  (totalBooked/totalListings)*100)

calendarHeat (groupedcalnedar$date, groupedcalnedar$percent_booked, ncolors=99, color ="g2r", varname= "Occupancy (Percentage) by Month")

?calendarHeat
install.packages("makeR")


##### USER REVIEW (TEXTUAL DATA) MINIG 

reviews1 <- read_csv('./Data/reviews.csv')

splitreview1column <- unlist(strsplit(as.character(reviews1$comments), split = ""))
reviewsWordDF <- data.frame("word" = splitreview1column)

wordDF <- reviewsWordDF %>%
  count(word, sort = TRUE) %>%
  ungroup()

library("tm")
docs <- Corpus(VectorSource(splitreview1column))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, c("we","it", "he", "this", "i", "the", "apartment","de", "un","us","well","es","5","la","2",""))

newcorpusdf <- data.frame(text=sapply(docs, identity), 
                          stringsAsFactors = F)
newcorpusdffiltered <- newcorpusdf %>% filter(text !="")
wordDF <- newcorpusdffiltered %>% count(text, sort = TRUE) %>%
  ungroup()

library(RColorBrewer)
library(wordcloud)
set.seed(789)
wordcloud(words = wordDF$text,
          freq = wordDF$n,
          min.freq = 1000,
          max.words = 500, colors = c("#e06f69","#357b8a", "#7db5b8", "#59c6f3"))

### Comment analysis using word cloud 

##Let's start by looking at the most common topics in the reviews; just creating a word cloud should enough. Wordclouds take a frequency count of the words in the corpus as input and produce a visually appealing representation of dominating (often occurring) words, with their size proportionate to their frequency. We have over a million reviews, thus we need to take a random sample, in this case 30,000 reviews. Despite the fact that the sampled dataset is minimal in contrast to the original, it meets our purpose well because we just need the basic terms here. As we'll see in the next section, further study of "good" and "negative" reviews will require more data.

r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)
install.packages("text2vec")
library(text2vec)
tokens <- space_tokenizer(as.character(reviews$comments))
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
# use window of 5 for context words 
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
glove = GlobalVectors$new(rank = 50, x_max = 10)
glove = GlobalVectors$new(rank = 50, x_max = 10)
# glove object will be modified by fit ()
word_vectors = glove$fit_transform(tcm, n_iter = 20)
word_vectors1 <- glove$components

### Building word vectors from Reviews

##The previously constructed word cloud is effective at locating what clients are looking for, but it is quite broad. Isn't it wonderful if we could find out what people think about the room sizes? Why don't you investigate what makes consumers "uncomfortable"?

library(tidyverse)
library(tm)
library(wordcloud)
install.packages("lwgeom", type = "source")
install.packages("tmap", dependencies = TRUE)
library(tmap)
library(lwgeom)
p1 = word_vectors["uncomfortable", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y =p1, method = "cosine", norm = "l2")
p1 = sort(cos_sim[,1], decreasing = TRUE)

df = data.frame(item = as.character(names(p1)), freq = as.numeric(p1))
df$item = gsub(",","", df$item)
df = df[!duplicated(df$item),]
set.seed(1234)
suppressWarnings(wordcloud(words = df$item, freq = df$freq, scale = c(2,0.2), 
                           max.words = 80, random.order = FALSE, rot.per = 0.2,
                           colors = c("#7db5b8", "#59c6f3", "#e06f69","#357b8a")))

### These are the most words associated with uncomfortable 

p2 = word_vectors["comfortable", , drop =FALSE]
cos_sim2 = sim2(x = word_vectors, y = p2, method = "cosine", norm = "l2")
p2 = sort(cos_sim2[,1], decreasing = TRUE)

df2 = data.frame(item = as.character(names(p2)), freq = as.numeric(p2))
df2$item = gsub(",","", df2$item)
df2 = df2[!duplicated(df2$item),]

set.seed(1234)
suppressWarnings(wordcloud(words = df2$item, freq = df2$freq, scale = c(2,0.2),
                           max.words = 80, random.order = FALSE, rot.per = 0.2,
                           colors = c("#7db5b8", "#59c6f3", "#e06f69","#357b8a")))

### These represent the good words pulled from the review 


#### Now analyzing the demand and Supply: Airbnb Customer Growth vs Listing Prices Over time 

 ggplot(calendarAll[year(calendarAll$date) == 2021,], aes(commonYear, averagePrice)) +
  geom_point(na.rm = TRUE, alpha=0.5, color = "#007A87") +
  geom_smooth(color = "#FF5A5F") +
  ggtitle("Is Price Fluctuation Seasonal?",
          subtitle = "Average listing price across months in 2021") +
  labs(x= "Month", y = "Average price across Listings") +
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold", color = "grey35"),
        plot.caption = element_text(color = "grey68")) +
  scale_x_date(labels = date_format("%b"))

 price2021  <- ggplot(calendarAll[year(calendarAll$date) == 2021,], aes(commonYear, averagePrice)) +
   geom_point(na.rm = TRUE, alpha=0.5, color = "#007A87") +
   geom_smooth(color = "#FF5A5F") +
   ggtitle("Is Price Fluctuation Seasonal?",
           subtitle = "Average listing price across months in 2021") +
   labs(x= "Month", y = "Average price across Listings") +
   theme(plot.title = element_text(face = "bold"), 
         plot.subtitle = element_text(face = "bold", color = "grey35"),
         plot.caption = element_text(color = "grey68")) +
   scale_x_date(labels = date_format("%b"))


 ### Which locations have better ratings?
 