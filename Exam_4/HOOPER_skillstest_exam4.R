### This is My Exam 4 which is a redo of my Exam 1

#Load Packages
library(tidyverse)

# 1: read in data
df <- read_csv("./cleaned_covid_data.csv")
df %>% glimpse

# 2: Subset the data to show just states that begin with A and save this as A States
A_states <- df[grepl("^A", df$Province_State),]
 

# 3: Create a plot of that subset showing active cases over time, which a seperate facet
# for each state

A_states %>%
  ggplot(aes(x=Last_Update,y=Active)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  facet_wrap(~Province_State,scales = "free")


# 4. Find the "peak of the Case_Fatlity_Ratio for each state save this as a new data frame
state_max_fataloty_rate <- df %>%
  group_by(Province_State) %>%
  summarise(Max_Fatality = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Max_Fatality))


# 5. Use that new data frame from task IV to create another plot.

state_max_fataloty_rate %>%
  ggplot(aes(x=reorder(Province_State,-Max_Fatality),y=Max_Fatality)) +
  geom_col() +
  theme(axis.text.x = element_text(angle=90,hjust = 1))

# 6. Using the FULL data set, plot cumulative deaths for the entire US over time
df %>%
  group_by(Last_Update) %>%
  summarise(Cumulative = sum(Deaths)) %>%
  ggplot(aes(x=Last_Update,y=Cumulative)) +
  geom_bar(stat= 'identity')
