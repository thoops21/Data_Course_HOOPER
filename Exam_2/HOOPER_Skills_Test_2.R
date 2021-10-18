# load packages 
library(tidyverse)
library(janitor)
library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(scales)

# plot 1 
# need the year, the land value (USD), the region
read_csv("./landdata-states.csv")
dat <- read_csv("./landdata-states.csv")

colnames(dat)
names(dat) <- make_clean_names(names(dat))
names(dat)

options(scipen = 999)

df <- dat %>%
  ggplot(aes(x=year, y=land_value, color = region)) +
  geom_smooth() +
  labs( x="Year",
        y="Land Value (USD)",
        color= "Region") +
  theme_minimal()

getwd()
jpeg("HOOPER_Fig_1.jpeg")
plot(df)
dev.off()

# 2 write some code to show which state(s) are found in the "NA" region         
new_dat <- dat[is.na(dat$region),]

#3 

# need to tidy the data into mortailty rate, year, continent 
df2 <- read_csv("unicef-u5mr.csv")
names(df2) <- make_clean_names(names(df2))
names(df2)



df3 <- df2 %>%
  pivot_longer(cols = starts_with("u5mr_"),
               names_to = "year",
               values_to = 'mortality_rate',
               names_prefix = "u5mr_",
               values_drop_na = TRUE) 

mortality_rate <- as.numeric(df3$mortality_rate)
year <- as.numeric(df3$year)
class(year)
class(mortality_rate)

df3 %>%
  ggplot(aes(x=year, y=mortality_rate, color = continent)) +
  geom_smooth() +
  geom_point() +
  labs( x="Year",
        y="MortalityRate",
        color = "Continent")+
   theme_minimal() 

jpeg("HOOPER_Fig_2.jpeg")
plot(df3)
dev.off()


# plot figure 3 
view(df3)

df4<- df3%>%
  group_by(continent, year) %>%
  summarise(mean = mean(mortality_rate), n = n())

df4 %>% 
  ggplot( aes(x=year , y=mean, color=continent )) +
  geom_point() +
  geom_line() +
  labs( x= "Year",
        y = "Mean Mortality Rate (death per 1000 live births",
        color = "Continent")

jpeg("HOOPER_Fig_3.jpeg")
plot(df4)
dev.off()


#creating plot number 4
df5 <- df3 %>% 
  group_by(country_name, region, mortality_rate, year) %>%
  summarise(mean =mean(mortality_rate)) %>%
  mutate(prop = mortality_rate/mean)
  
df5 %>%
  ggplot(aes(x=year,  y = mean)) +
  geom_point(color = "blue", size = 0.5) +
  facet_wrap( ~ region) +
  theme_minimal() +
  labs( x= "Year",
        y= "Mortality Rate")

jpeg("HOOPER_Fig_4.jpeg")
plot(df5)
dev.off()


  
 


 







