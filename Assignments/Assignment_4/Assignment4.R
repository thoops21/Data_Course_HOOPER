library(tidyverse)
library(ggplot2)

options(scipen = 999)
listings <- read_csv("./Data/listings_1 copy.csv")
reviews <- read_csv("./Data/reviews copy.csv")
calendar <- read_csv("./Data/calendar copy.csv")
neighborhoods <- read_csv("./Data/neighborhoods copy.csv")


### Demand and Price Analysis
# Look at the demand over the years since the beginning Airbnbs in the DC area
# look at the relationship of price vs demand = do prices of listings flucutate with demand, how do prices vary by days of the week
# To find the demand, will use number of reviews as the indicator for demand

reviewsNUM <- reviews %>% 
  group_by(date = reviews$date) %>%
  summarise(number= n())

ggplot(reviewsNUM, aes(date,number))+
  geom_point(na.rm = TRUE, color = "blue", alpha=0.5) +
  geom_smooth(color= "#FF5A5F") +
  ggtitle("How popular is Airbnb?",
          subtitle = "Number of Reviews Across Years") +
  labs(x= "Year", y= "Unique Listings Recieving Reviews") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68")) 
 
