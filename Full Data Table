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

View(Final_Table)
