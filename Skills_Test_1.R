THis file is my test1
# make sure I am in the right file path
getwd()

# step 1 turn csv into data frame
read.csv("cleaned_covid_data.csv")
cleanedcoviddata <- read.csv("cleaned_covid_data.csv")

# step 2 subset data 
A <- subset(cleanedcoviddata, grepl("A", cleanedcoviddata$Province_State))

# 3 create a scatterplot of the subset of data using the active cases
library(tidyverse)

df <- A
df %>%
ggplot(aes(x=Last_Update, y=Active, color=Province_State)) +
  geom_point() +
  geom_smooth(method = "lm", se= FALSE) +
  labs( x="Time", y="Active Cases") +
  facet_wrap(~  Province_State, scales = "free")


# 4 
df2 <- cleanedcoviddata
df4 <- data.frame(cleanedcoviddata$Province_State, cleanedcoviddata$Case_Fatality_Ratio)
library(dplyr)
state_max_fatality_rate <- df4 %>% group_by(cleanedcoviddata.Province_State) %>% top_n(1,cleanedcoviddata.Case_Fatality_Ratio)
arrange(state_max_fatality_rate, desc(state_max_fatality_rate$cleanedcoviddata.Case_Fatality_Ratio))
state_max_fatality_rate <- arrange(state_max_fatality_rate, desc(state_max_fatality_rate$cleanedcoviddata.Case_Fatality_Ratio))

#5
Region <- as.factor(state_max_fatality_rate$cleanedcoviddata.Province_State)
Maximum_Fatality_Ratio <- as.numeric(state_max_fatality_rate$cleanedcoviddata.Case_Fatality_Ratio)

ggplot(data = state_max_fatality_rate, aes(x= reorder(Region,-Maximum_Fatality_Ratio)))  +
  geom_bar() +
  labs(x = "Region", y = "Maximum_Fatality_Ration") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
 

       
=
