# Call on relevant packages
library(tidyverse)
library(dplyr)
library(ggplot2)

# Setting working directory so R can access the data sets
setwd("/Users/hughparry/Downloads/IDS Raw Data")

#
#
#
#
#

# Read CSV file for Continents data
Continents <- read.csv("continents-according-to-our-world-in-data.csv")

# Arranging by continents
Continents <- Continents %>% arrange(Continent)

# Removing unwanted columns
Continents <- Continents %>% select(-Code, -Year)

#
#
#
#
#

# Read CSV file for Youth NEET data
Youth_NEET <- read.csv("youth-not-in-education-employment-training.csv")

# Joining data tables for Continents and Youth_NEET
Youth_NEET_Continents <- Youth_NEET %>% full_join(Continents, by = "Entity")

# Remove all values which have now data (N/A) for Continent
Youth_NEET_Continents <- Youth_NEET_Continents %>% filter(!is.na(Continent))

#
#
#
#
#

# Reordering the file
Youth_NEET_Continents <- Youth_NEET_Continents %>% arrange(Continent, Year)

# Renaming Youth NEET column, and Entity column
Youth_NEET_Continents <- Youth_NEET_Continents %>% rename(
      Youth_NEET = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.,
      Country = Entity)

#
#
#
#
#

# Read CSV file for GDP Per Capita data
gdp_per_capita <- read.csv("gdp-per-capita-worldbank.csv")

# Rename Entity column to Country
gdp_per_capita <- gdp_per_capita %>% rename(Country = Entity)

#Rename GDP per capita column
names(gdp_per_capita)[4] <- "GDP_Per_Capita"

# Convert from strings to numbers
gdp_per_capita <- gdp_per_capita %>% mutate(Year = as.numeric(Year), GDP_Per_Capita = as.numeric(GDP_Per_Capita))

# Remove any N/A vlues that may be present
gdp_per_capita <- gdp_per_capita %>% filter(!is.na(Year), !is.na(GDP_Per_Capita))

# Arrange by Country and Year
gdp_per_capita <- gdp_per_capita %>% arrange(Country, Year)

#
#
#
#
#

# Read CSV file for Population data
population <- read_csv("population-with-un-projections.csv")

# Rename first column to "Country"
population <- population %>% rename(Country = Entity)

# Rename fourth column to Population
names(population)[4] <- "Population"

# Remove predicted population
population <- population %>% mutate("Population - Sex: all - Age: all - Variant: medium" = NULL)

#Remove all N/A in Population column
population <- population %>% filter(!is.na(Population))

#Convert to numbers and remove missing values
population <- population %>% mutate(
  Year = as.numeric(Year), 
  Population = as.numeric(Population)) 

# Remove any possible N/A values
population <- population %>% filter(!is.na(Year), !is.na(Population)) %>% arrange(Country, Year)

#
#
#
#
#

## Join all data tables

# Joining data for Youth NEET, Continents, and GDP Per Capita
Youth_NEET_Continents_GDP <- full_join(Youth_NEET_Continents, gdp_per_capita)

# Joining data for Youth NEET, Continents, GDP Per Capita and Population
All_Data <- full_join(Youth_NEET_Continents_GDP, population)

# Remove all N/A values
All_Data <- All_Data %>% filter(!is.na(Code), 
                                !is.na(Year), 
                                !is.na(Youth_NEET), 
                                !is.na(Continent), 
                                !is.na(GDP_Per_Capita), 
                                !is.na(Population))

# Remove all data for Antarctica
All_Data <- All_Data %>% filter(!Continent == "Antarctica")

# Turn strings into numbers for Youth NEET, Year, and GDP Per Capita
All_Data <- All_Data %>% mutate(
  Youth_NEET = as.numeric(Youth_NEET), 
  Year = as.numeric(Year), 
  GDP_Per_Capita = as.numeric(GDP_Per_Capita), 
  Population = as.numeric(GDP_Per_Capita))

#
#
#
#
#

# Filter data so that all data is since 2000, and has a Youth NEET below 55 (remove outliers)
Restricted_Data <- All_Data %>% filter(Year > 1999, Youth_NEET < 56)
       
#
#
#
#
#

# Graph of NEET over Time
Restricted_Data %>% ggplot(aes(x = Year, y = Youth_NEET, colour = Continent))+
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
# 

## Create a Bar Plot for the change in Youth NEET percentage between 2000 and 2020

# Filter for data that is only in years 2000 and 2020
Restricted_Data2 = Restricted_Data %>% filter(Year == 2000 | Year == 2020)

# Group data by Continent and Year, and compute mean NEET for each continent each year
Restricted_Data2 <- Restricted_Data2 %>% group_by(Continent, Year) %>% 
  summarise(Mean_NEET = mean(Youth_NEET))

# calculate the change in mean NEET percentage for each continent over the period
Restricted_Data2 <- Restricted_Data2 %>% mutate(NEET_Change = Mean_NEET - lag(Mean_NEET))

# filter out all N/A values
Restricted_Data2 <- Restricted_Data2 %>% filter(!is.na(NEET_Change))

# now make the graph
Restricted_Data2 %>% ggplot(aes(x = Continent, y = NEET_Change)) + #This makes a graph with continents on the x-axis and our calculated NEET change on the y-axis
  geom_col(aes(fill = Continent == Continent[1:3])) + #This means that it is a column plot
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), guide = "none")+ #This colours the first three columns gree, and the final three red
  theme_minimal()+ #Just changes the look of the graph to make it more appealing
  labs(title = "Change in Youth NEET Rate Between 2000 and 2020",
       x = "Continent",
       y = "Change in Youth NEET Rate (%)",
       caption = "Green = Improvement, Red = Worse")+ #Adding labels to the graph
  geom_text(aes(label = round(NEET_Change, 2)), 
            vjust = -0.3) #Adding the value of each change to the relevant column
  
#
#
#
#
#

## Lets now try and graph Youth NEET weighted by population over time
# group data by continent and year
Restricted_Data3 = Restricted_Data %>% group_by(Continent, Year)

# calculate NEET multiplied by country population and make a new column to store it
NEETxPop = Restricted_Data3 %>% mutate(NEETxPop = Youth_NEET * Population)

# Now divide our value for (NEET x country population) by continent population to get our value for weighted NEET
Weighted_NEET <- NEETxPop %>%
  group_by(Continent, Year) %>%
  mutate(Weighted_NEET = NEETxPop / sum(Population)
  )

#
#
#
#
#

# Now plot the graph for weighted youth NEET over time
Weighted_NEET %>% ggplot(aes(x = Year, y = Weighted_NEET, colour = Continent))+
  geom_smooth(se = FALSE)+
  theme_minimal()+
  labs(title = "Youth NEET over time (weighted by population)", 
       x = "Year (2000-2022)",
       y = "Youth NEET Weighted by Population")

#
#
#
#
#

##´now the bar plot  for this data
# filter so we only have data of weighted NEET for years 2000 and 2020
Weighted_NEET2 = Weighted_NEET %>% filter(Year == 2000 | Year == 2020)

# Group data by Continent and Year, and compute weighted mean NEET for each continent each year
Weighted_NEET2 <- Weighted_NEET2 %>% 
  group_by(Continent, Year) %>% 
  summarise(Mean_Weighted_NEET = mean(Weighted_NEET))

# calculate the change in weighted NEET percentage between 2000 and 2020 and store in new column
Weighted_NEET2 = Weighted_NEET2 %>% mutate(Weighted_NEET_Change = Mean_Weighted_NEET - lag(Mean_Weighted_NEET))

# filter out all N/A values
Weighted_NEET2 <- Weighted_NEET2 %>% filter(!is.na(Weighted_NEET_Change))

# Now lets make the bar graph
Weighted_NEET2 %>% ggplot(aes(x = Continent, y = Weighted_NEET_Change)) + #This productes a graph with continents on the x-axis and change in weighted neet on the y-axis 
  geom_col(aes(fill = Continent == Continent[1:6])) + #This means that is it a column plot
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "green"), guide = "none")+ #This makes all columns coloured green (yes it is ugly but I copy and pasted the first graph)
  labs(title = "Change in Weighted Youth NEET Rate Between 2000 and 2020",
       x = "Continent",
       y = "Change in Weighted Youth NEET Rate (%)",
       caption = "Green = Improvement, Red = Worse")+ #Adding labels to the graph
  theme_minimal()+ #Changes the look of the graph to make it more appealing
  geom_text(aes(label = round(Weighted_NEET_Change, 2)),   # label = the value
            vjust = -0.3) #this adds the value of the change in weighted neet to the relevant continents bar




# Thank you for reading this code... It took me ages.
