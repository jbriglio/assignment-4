library(tidyverse)
library(dplyr)
library(haven)
library(foreign)
library(stringr)
library(tidyr)
tidyr::who
#making who a tidy data set
View(who)
who1 <- who %>% gather(new_sp_m014:newrel_f65, key="key", value="cases", na.rm=TRUE)
who1 %>% count("key")
who2 <- who1 %>% mutate(key=str_replace(key, "newrel", "new_rel"))
who3 <- who2 %>% separate(key, c("new", "type", "sexage"), sep="_")
who4 <- who3 %>% select(-new, -iso2, -iso3)
who5 <- who4 %>% separate(sexage, c("sex", "age"), sep=1)

#12.6.1 question #3
who6 <- select(who1, country, iso2, iso3) %>%
  distinct() %>%
  group_by(country) %>%
  filter(n() > 1)
who6
#the tibble is empty because there are repeated names for countries

#12.6.1 question #4
?count
?ggplot
who7 <- who5 %>% group_by(country, year, sex) %>% count(wt = cases)
who7 %>% ggplot(aes(year, n)) + geom_line(aes(group = country, color = sex))

#10.5 question #5
?tibble::enframe()
#it converts a vector/list into a 2 column data frame 
#you would use it when you are trying to create a date frame that contains a variable
#and a matching value 
#example
enframe(c(a=1, b=2, c=3))

#table 4 to table 6 
pew <- read.spss("pew.sav")
pew <- as.data.frame(pew)

religion <- pew[c("q16", "reltrad", "income")]
religion$reltrad <- as.character(religion$reltrad)
religion$reltrad <- str_replace(religion$reltrad, " Churches", "")
religion$reltrad <- str_replace(religion$reltrad, " Protestant", " Prot")
religion$reltrad[religion$q16 == " Atheist (do not believe in God) "] <- "Atheist"
religion$reltrad[religion$q16 == " Agnostic (not sure if there is a God) "] <- "Agnostic"
religion$reltrad <- str_trim(religion$reltrad)
religion$reltrad <- str_replace_all(religion$reltrad, " \\(.*?\\)", "")

religion$income <- c("Less than $10,000" = "<$10k", 
                     "10 to under $20,000" = "$10-20k", 
                     "20 to under $30,000" = "$20-30k", 
                     "30 to under $40,000" = "$30-40k", 
                     "40 to under $50,000" = "$40-50k", 
                     "50 to under $75,000" = "$50-75k",
                     "75 to under $100,000" = "$75-100k", 
                     "100 to under $150,000" = "$100-150k", 
                     "$150,000 or more" = ">150k", 
                     "Don't know/Refused (VOL)" = "Don't know/refused")[religion$income]

religion$income <- factor(religion$income, levels = c("<$10k", "$10-20k", "$20-30k", "$30-40k", "$40-50k", "$50-75k", 

                                                                                                            "$75-100k", "$100-150k", ">150k", "Don't know/refused"))
?count
religion %>% count(reltrad)
religion %>% count(q16)
r.1 <- religion %>% count(reltrad)
r.2 <- religion %>% count(q16)
r.3 <- religion %>% count(income)
r.4 <- religion %>% group_by(reltrad, q16, income) %>% count(income)
r.5 <- r.4[c("reltrad", "income", "n")]
r.6 <- r.5 %>% dplyr::rename(freq = n)
View(r.6)

#table 7 to 8 
billboard <- read_csv("billboard.csv")
b.1 <- billboard %>% gather(key="week", value = "rank", -year, -artist.inverted, -track, -time, -genre, -date.entered, -date.peaked)
b.2 <- b.1 %>% select(year, artist=artist.inverted, time, track, date = date.entered, week, rank )
b.3 <- b.2 %>% arrange(track)
b.4 <- b.3 %>% filter(!is.na(rank))
b.5 <- b.4 %>% separate(week, into=c("A", "B", "C"), sep=c(1, -7), convert=TRUE)
b.6 <- b.5 %>% select(-A, -C)
b.7 <- b.6 %>%   dplyr::rename(week = B)
b.8 <- b.7 %>% arrange(artist, track)
View(b.8)