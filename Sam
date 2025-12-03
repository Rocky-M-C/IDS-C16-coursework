library(tidyverse)
library(dplyr)

#Reading the CSV file 
continents <- read.csv('/Users/katar/Downloads/data sets/continents-according-to-our-world-in-data.csv')

head(continents)

#arranging by continents and removing unwanted columns 
continents <- continents %>%
  arrange(Continent) %>%
  select(-Code, -Year)

view(continents)

#reading CSV file
Youth_NEET <- read.csv('/Users/katar/Downloads/data sets/youth-not-in-education-employment-training.csv')
head(Youth_NEET)

#joining continents and Youth_NEET
Youth_NEET <- Youth_NEET %>%
  left_join(continents, by = 'Entity')

#reordering the file to get the clean dataset and renaming a column to make it tidier 
Youth_NEET <- Youth_NEET %>%
  arrange(Continent, Year) %>%
  rename(YNEET = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.)

view(Youth_NEET)
