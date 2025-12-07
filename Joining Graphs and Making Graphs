library(tidyverse)
library(dplyr)

#Reading the CSV file 

setwd("/Users/hughparry/Downloads/IDS Raw Data")
continents <- read.csv("continents-according-to-our-world-in-data.csv")
head(continents)

#arranging by continents and removing unwanted columns 

continents <- continents %>%
  arrange(Continent) %>%
  select(-Code, -Year)
view(continents)

#reading CSV file

Youth_NEET <- read.csv("youth-not-in-education-employment-training.csv")
head(Youth_NEET)

#joining continents and Youth_NEET

Youth_NEET <- Youth_NEET %>%
  left_join(continents, by = 'Entity')

#reordering the file to get the clean dataset and renaming a column to make it tidier 

Youth_NEET <- Youth_NEET %>%
  arrange(Continent, Year) %>%
  rename(YNEET = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.)

# Remane Entity to Country

names(Youth_NEET)[1] <- "Country"
view(Youth_NEET)

#
#
#
#
#

#gdp per cpita, reading the CSV file 

gdp_per_capita <- read.csv("gdp-per-capita-worldbank.csv")
View(gdp_per_capita)

#Rename first column to "country"

names(gdp_per_capita)[1] <- "Country"

#Clean country names

gdp_per_capita <- gdp_per_capita %>% mutate(Country = str_trim(Country)) %>% filter(Country != "", !is.na(Country))

#Rename GDP per capita column

names(gdp_per_capita)[4] <- "GDP_Per_Capita"
View(gdp_per_capita)

#Convert to numbers and remove missing values and arrange the data

gdp_per_capita <- gdp_per_capita %>%
  mutate(Year = as.numeric(Year), GDP_Per_Capita = as.numeric(GDP_Per_Capita)) %>% filter(!is.na(Year), !is.na(GDP_Per_Capita)) %>%
  arrange(Country, Year)

#Show results
View(gdp_per_capita)

#
#
#
#
#

#Population, Read the raw CSV file uploaded

population <- read_csv("population-with-un-projections.csv")

#Rename first column to "Country"

names(population)[1] <- "Country"
View(population)

#Rename fourth column to Population
names(population)[4] <- "Population"

#Clean country names

population <- population %>% 
  mutate(Country = str_trim(Country)) %>%
  filter(Country != "", !is.na(Country))

#Remove predicted population

population <- population %>% mutate("Population - Sex: all - Age: all - Variant: medium" = NULL)
View(population)

#Remove all N/A in Population column

population <- population %>% filter(!is.na(Population))
View(population)

#Convert to numbers and remove missing values

population <- population %>%
  mutate(Year = as.numeric(Year), Population = as.numeric(Population)) %>% filter(!is.na(Year), !is.na(Population)) %>% arrange(Country, Year)

#Show results

View(population)

#
#
#
#
#

# Join tables

NEET_GDP <- left_join(Youth_NEET, gdp_per_capita)
Final_Table <- left_join(NEET_GDP, population)
Final_Table <- Final_Table %>% 
  filter(!is.na(Continent))

Final_Table <- Final_Table %>% mutate(YNEET = as.numeric(YNEET), Year = as.numeric(Year))

View(Final_Table)

Final_Table <- Final_Table %>% filter(Year > 1999, YNEET < 55)

View(Final_Table)
                                
# Create graphs:
# Graph of NEET over Time
library(ggplot2)
Final_Table %>% ggplot(aes(x = Year, y = YNEET, colour = Continent))+
  geom_smooth(se = FALSE)+
  theme_minimal()+
  labs(title = "Youth NEET Over Time",
       x = "Year (2000-2022)",
       y = "Youth NEET"
  )
  

#
#
#
#
#
# now lets make that bar plot thing

Final_Table2 = Final_Table %>% filter(Year == 2000 | Year == 2020)
View(Final_Table2)

Final_Table2 <- Final_Table2 %>% group_by(Continent, Year) %>% summarise(mean_neet = mean(YNEET))
View(Final_Table2)
# change in NEET%
Final_Table3 = Final_Table2 %>% mutate(ChangeInNEET = mean_neet - lag(mean_neet))
  
Final_Table3 <- Final_Table3 %>% filter(!is.na(ChangeInNEET))
View (Final_Table3)


Final_Table3 %>% ggplot(aes(x = Continent, y = ChangeInNEET)) +
  geom_col(aes(fill = Continent == Continent[1:3])) +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), guide = "none")+
  labs(title = "Change in NEET Rate (2000-2020)",
       x = "Continent",
       y = "Change in NEET Rate (%)",
       caption = "Green = Improvement, Red = Worse")+
  geom_text(aes(label = round(ChangeInNEET, 2)),   # label = the value
            vjust = -0.3)
  
  
  
  
  
  





# try again
View(Final_Table)

Final_grouped = Final_Table %>% group_by(Continent, Year)

neet_times_pop = Final_grouped %>% mutate(neet_times_pop = YNEET * Population)

View(neet_times_pop)


weighted_neet <- neet_times_pop %>%
  group_by(Continent, Year) %>%
  mutate(weighted_neet = neet_times_pop / sum(Population)
  )

weighted_neet_2000 = weighted_neet %>% filter(Year > 1999)

View(weighted_neet_2000)


# now plot it:

weighted_neet_2000 %>% ggplot(aes(x = Year, y = weighted_neet, colour = Continent))+
  geom_smooth(se = FALSE)+
  geom_point(size = 0.01)+
  theme_minimal()+
  labs(title = "Youth NEET over time (weighted by population)", 
       x = "Year (2020-2022)",
       y = "Youth NEET Weighted by Population")

# had to cut off years before 2000 as didnt have all data for countries so this distored the data massively






#´now the bar plot thing for this data:
weighted_neet_20002 = weighted_neet_2000 %>% filter(Year == 2000 | Year == 2020)
View(weighted_neet_20002)

weighted_neet_20002 <- weighted_neet_20002 %>% group_by(Continent, Year) %>% summarise(mean_weighted_neet = mean(weighted_neet))
View(weighted_neet_20002)

# change in NEET%
weighted_neet_20003 = weighted_neet_20002 %>% mutate(ChangeInWeightedNEET = mean_weighted_neet - lag(mean_weighted_neet))

weighted_neet_20003 <- weighted_neet_20003 %>% filter(!is.na(ChangeInWeightedNEET))
View (weighted_neet_20003)


weighted_neet_20003 %>% ggplot(aes(x = Continent, y = ChangeInWeightedNEET)) +
  geom_col(aes(fill = Continent == Continent[1:6])) +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), guide = "none")+
  labs(title = "Change in Weighted NEET Rate (2000-2020)",
       x = "Continent",
       y = "Change in Weighted NEET Rate (%)",
       caption = "Green = Improvement, Red = Worse")+
  geom_text(aes(label = round(ChangeInWeightedNEET, 2)),   # label = the value
            vjust = -0.3)


