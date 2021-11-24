#load packages

library(tidyverse)
library(janitor)
library(readxl)
library(tidyr)
library(dplyr)
library(transformr)
library(broom)
library(sf)
library(magrittr)
library(lattice)

# Part 1
# Create box plot, with three facets based on salary vs rank, but first need to tidy the data based on our three observations within tier 

#load data 
read_csv("./FacultySalaries_1995.csv")
dat <- read_csv("./FacultySalaries_1995.csv")

names(dat)
colnames(dat) <- make_clean_names(colnames(dat))
names(dat)


dat <-  dat %>%
  pivot_longer(cols = c("num_full_profs", "num_assoc_profs", "num_assist_profs", "num_instructors"),
               values_to = "staff_num",
               names_to = "rank_num") %>%
  pivot_longer(cols = c("avg_assist_prof_comp", "avg_assoc_prof_comp", "avg_full_prof_comp"),
               values_to = "compensation",
               names_to = "rank_comp") %>%
  pivot_longer(cols = c("avg_assist_prof_salary", "avg_assoc_prof_salary", "avg_full_prof_salary"),
               values_to = "salary",
               names_to = "rank") %>%
  mutate(rank = case_when( rank == "avg_assist_prof_salary" ~ "Assist",
                           rank == "avg_full_prof_salary" ~ "Full",
                           rank == "avg_assoc_prof_salary" ~ "Assoc"))

dat <- subset(dat, tier!="VIIB")
view(dat)

dat %>%
  ggplot(aes(x=rank, y=salary)) +
  geom_boxplot(aes(fill=rank)) +
  theme_minimal() +
  facet_wrap( ~tier) +
  theme(legend.key.size = unit(0.75, "cm"),
        axis.title.x = element_text(size = 15, color = "black"),
        axis.title.y = element_text(size = 15, color = "black"), 
        axis.text.x = element_text(angle =  65, size= 12, color= "black"),
        axis.text.y = element_text(size = 12, color = "black"))

ggsave("./HOOPER_Fig_1.jpg")

# Part 2
# Create an ANOVA table that conducts analysis of salary compared to state, tier, rank 

summary(dat)

mod <- glm(data = dat,
           formula = salary ~ state + tier + rank)
summary(mod)

anova(mod)
mod1 <- anova(mod)
capture.output(mod, file = "./Salary_ANOVA_Summary.txt")

# Part 3
# First is to read in the data, then to tidy the data, mainly to pivot longer to just get the chemical and its concentration number 

read_csv("./Juniper_Oils.csv")
dat1 <- read_csv("./Juniper_Oils.csv")

dat1 <- dat1 %>%
  pivot_longer(cols = c("alpha-pinene","para-cymene","alpha-terpineol",
                        "cedr-9-ene","alpha-cedrene","beta-cedrene","cis-thujopsene",
                        "alpha-himachalene","beta-chamigrene","cuparene","compound 1",
                        "alpha-chamigrene","widdrol","cedrol","beta-acorenol",
                        "alpha-acorenol","gamma-eudesmol","beta-eudesmol",
                        "alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol",
                        "compound 2","thujopsenal"),
               values_to = "concentration",
               names_to = "chemicalID")

# Part 4
#  Make me a graph of the following: x = YearsSinceBurn, y = Concentration, 
# facet = ChemicalID (use free y-axis scales)

dat1 %>%
  ggplot(aes(x=YearsSinceBurn, y=concentration)) +
  geom_smooth(method = loess) +
  theme_minimal() +
  facet_wrap(~chemicalID, scales = "free_y")

ggsave("./HOOPER_fig_1.jpg") 

# Part 5
# Use a generalized linear model to find which chemicals show concentrations 
#that are significantly (significant, as in P < 0.05) affected by "Years Since Burn". 
#Use the tidy() function from the broom R package in order to produce a data frame 
#showing JUST the significant chemicals and their model output 
# (coefficient estimates, p-values, etc) 

mod2 <- glm(data = dat1, formula = concentration ~ chemicalID* YearsSinceBurn)
summary(mod2)

mod2tidy <- tidy(mod2)





