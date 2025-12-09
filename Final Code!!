# Call on relevant packages
library(tidyverse)
library(dplyr)
library(ggplot2)

# Setting working directory so R can access the data sets
setwd("/Users/katar/Downloads/data sets")

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
population <- read_csv("/Users/katar/Downloads/population-with-un-projections.csv")

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
Final_Table <- All_Data %>% mutate(
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
Restricted_Data <- Final_Table %>% filter(Year > 1999, Youth_NEET < 56)

#
#
#
#

#Adding LDC's

ldc_list <- c('Angola', 'Benin', 'Burkina Faso', 'Burundi', 'Central African Republic', 'Chad', 'Comoros', 
              'Democratic Republic of Congo', 'Djibouti', 'Eritrea', 'Ethiopia', 'Gambia', 'Guinea', 
              'Guinea-Bissau', 'Lesotho', 'Liberia', 'Madagascar', 'Malawi', 'Mali', 'Mauritania', 'Mozambique', 
              'Niger', 'Rwanda', 'Senegal', 'Sierra Leone', 'Somalia', 'South Sudan', 'Sudan', 'Togo', 
              'Uganda', 'Tanzania', 'Zambia', 'Afghanistan', 'Bangladesh', 'Cambodia', 
              'Laos', 'Myanmar', 'Nepal', 'East Timor', 'Yemen', 'Haiti', 'Kiribati', 
              'Solomon Islands', 'Tuvalu')

#checking if the list has all values 
length(ldc_list)

#adding a column for least developed countries, 1 if the country is LDC and 0 if not
Final_Table <- Final_Table %>%
  mutate(LDC = case_when(
    Country %in% ldc_list ~ 1,
    TRUE ~ 0
  ))


# ------------------------------------------------------------------------

#QUESTION 1 ANALYSIS

#heading the data 

head(Final_Table)
# creating GDP number by multiplying GDP and Population

Final_Table <- Final_Table %>% 
  mutate(GDP = GDP_Per_Capita*Population)

#arranging data so that I can calculate growth rate

Final_Table <- Final_Table %>%
  arrange(Country, Year)

#creating a compound anuual growth rate to deal with gaps in data

Final_Table <- Final_Table %>%
  group_by(Country)%>%
  mutate(
    
    # creates a variable for the number of years between two data points
    
    Years_Gap = Year - lag(Year),
    
    # Averages Growth rate over given time period
    
    Growth_Rate_GDP = (GDP / lag(GDP))^(1 / Years_Gap)- 1 
  )%>%
  ungroup()

# The goal of this table is to interpolate the data between data points

# first I have to do the same thing I did with GDP growth  to fill the multi year gaps in population

Final_Table <- Final_Table %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    
    # creates a variable for the number of years between two data points
    
    Years_Gap = Year - lag(Year),
    
    # Averages Growth rate over given time period
    
    Growth_Rate_POP = (Population / lag(Population))^(1 / Years_Gap)- 1 
  ) %>%
  ungroup()
# I need to create new rows, Im doing this at the end so some things later will be redundent

Final_Table <- Final_Table %>% 
  #filter out long NA gaps, because initial will be NA 
  
  mutate(Years_Gap = ifelse(is.na(Years_Gap) | Years_Gap < 1, 1, Years_Gap )) %>%
  
  #creating as many rows as spesified in year gap unless its 1
  
  uncount(weights = as.integer(Years_Gap), .remove = FALSE, .id = "counter") %>% 
  
  mutate(
    #re-labling the newly created years correctly 
    Year = Year - (counter - 1), 
    
    #interpolate the data
    GDP = GDP / ((1 + Growth_Rate_GDP) ^ (counter -1)),
    Population = Population / ((1 + Growth_Rate_POP) ^ (counter -1)),
    
    #this is a fix I did at the end so I didnt have to solve several things at once
    #becuase of this I will just set Years_Gap = 1 so that my code still works
    Years_Gap = 1 
  ) %>%
  #
  select(-counter) 


#finding the distance between growth rate and target

LDC_Table <- Final_Table %>%
  
  #Remove all non LDC countries
  
  filter(LDC == 1) %>%
  
  #Remove the initial year for growth rate because growth is NA 
  
  filter(!is.na(Growth_Rate_GDP)) %>%
  
  #Calculating yearly misses
  
  mutate(Target_Miss = Growth_Rate_GDP - 0.07) %>%
  
  group_by(Country, Continent) %>%
  
  # Averaging misses for each country, but accounts for gaps in data by using weighting
  
  summarise(
    Mean_Miss = weighted.mean(Target_Miss, w = Years_Gap, na.rm = TRUE))

#Success hitting the 7% target plot

# Need to re-order the mean distances becuase it need to be decending

Suc_Bar <- ggplot(LDC_Table, aes(x= reorder(Country, Mean_Miss), y= Mean_Miss, fill = Continent)) + 
  geom_col() + 
  
  #makes it readable by flipping it 
  coord_flip() + 
  
  geom_hline(yintercept = 0, colour = "black", linewidth = 1) + 
  labs(
    title= "Weighted Average Differnce to 7% Target" , 
    x = "Country Name" ,
    y = "Weighted Average Difference" ,
    fill = "continent" 
  ) + 
  theme_minimal() 

print(Suc_Bar)


# second bit of anlysis looking at how much growth is distorted by population changes

# now I am going to calculate the coninental weighted rates for how much population growth effects GDP 

Cont_table <- Final_Table %>%
  #remvoed non LDC's and got rid of intial years with NA results
  filter(LDC ==1) %>%
  filter(!is.na(Growth_Rate_GDP)) %>%
  filter(!is.na(Growth_Rate_POP)) %>%
  group_by(Continent, Year) %>% 
  summarise(
    # create weighted Conintental GDP growth rate and population numbers
    Weighted_Cont_GDP_growth = weighted.mean(Growth_Rate_GDP, w = GDP, na.rm = TRUE),
    
    Weighted_Cont_POP_growth = weighted.mean(Growth_Rate_POP, w = Population, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    #Net gain on gdp ajusted for population 
    Net_PC_Growth = (1 + Weighted_Cont_GDP_growth)/(1 + Weighted_Cont_POP_growth) -1 
  )

Plot_GDP_gain <- ggplot(Cont_table, aes(x = Year)) + 
  # Total GDP Growth
  geom_col(aes(y = Weighted_Cont_GDP_growth, fill = "Weighted GDP Growth"), alpha = 0.5) +
  
  #population drag 
  geom_line(aes(y = Weighted_Cont_POP_growth, colour = "Weighted Population Growth"), linewidth = 1, linetype = "dashed") +
  
  #Real Standard of Living
  geom_line(aes(y = Net_PC_Growth, color = "Real Per Capita Growth"), linewidth = 1.2) + 
  
  #Target line
  geom_hline(yintercept = 0.07, linetype = "dotted", linewidth = 1 ) + 
  
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c("Weighted GDP Growth" = "blue")) + 
  scale_colour_manual(values = c("Weighted Population Growth" = "orange", "Real Per Capita Growth" = "green")) + 
  
  facet_wrap(~Continent) + 
  
  labs(
    title = "Regional Growth Analysis (Weighted Average)", 
    subtitle = "Effect of Popultion Drag: Blue Bar - Orange Line = Green Line" , 
    y = "Annual Growth Rate" ,
    fill= NULL , 
    colour = NULL
  ) + 
  theme_minimal() + 
  theme(legend.position = "bottom")

print(Plot_GDP_gain)

# part 3 lets look if the LDC's are catching up with the rich world

Rich_Table <- Final_Table %>%
  filter(LDC == 0) %>%
  filter(!is.na(Growth_Rate_GDP))%>%
  group_by(Year) %>%
  #rank countries by per-capita terms
  mutate(Rich_rank = rank(desc(GDP_Per_Capita))) %>% 
  filter(Rich_rank <= 35) %>% 
  summarise(
    #calculate GDP per capita growth rate
    Rich_PC_GR = ((1 + weighted.mean(Growth_Rate_GDP, w = GDP, na.rm = TRUE)) /
                    (1 + weighted.mean(Growth_Rate_POP, w = Population, na.rm = TRUE))) -1, 
    .groups = "drop"
  )


LDC_Table_Growth <- Final_Table %>%
  filter(LDC == 1) %>%
  filter(!is.na(Growth_Rate_GDP) & !is.na(Growth_Rate_POP))%>%
  
  group_by(Year) %>%
  summarise(
    #calculate GDP per capita growth rate
    LDC_PC_GR = ((1 + weighted.mean(Growth_Rate_GDP, w = GDP, na.rm = TRUE)) /
                   (1 + weighted.mean(Growth_Rate_POP, w = Population, na.rm = TRUE))) -1, 
    .groups = "drop"
  )

#joining the tables

Convergence_Table <- left_join(LDC_Table_Growth, Rich_Table, by = "Year") %>%
  mutate(
    # calculate the gap between LDC and Rich growth rate 
    Growth_Gap = LDC_PC_GR - Rich_PC_GR, 
    Status = ifelse(Growth_Gap > 0, "LDCs Converging", "LDCs Diverging")
  )%>% 
  filter(!is.na(Growth_Gap))

# making the graph 

Plot_convergence <- ggplot(Convergence_Table, aes(x = Year, y = Growth_Gap, fill = Status)) + 
  geom_col() + 
  geom_hline(yintercept = 0 , colour = "black", linewidth = 1 ) + 
  
  geom_text(aes(label = scales::percent(Growth_Gap, accuracy = 0.1)), 
            vjust = ifelse(Convergence_Table$Growth_Gap > 0, -0.5, 1.5),
            size = 3) +
  
  scale_fill_manual(values = c("LDCs Converging" = "green", "LDCs Diverging" = "red")) + 
  scale_y_continuous(labels = scales::percent) + 
  
  labs(
    title = "Global Convergence Test" , 
    subtitle = "Difference in Growth: All LDCs vs. Top 35 Richest Economies", 
    y = "Growth Gap (LDC Rate - Rich Rate)", 
    fill = NULL 
  )+ 
  theme_minimal() + 
  theme(legend.position = "bottom")

print(Plot_convergence)

# plotting the growth between the richest 35 and the LDC's on an indexed graph

GDP_Index_Table <- full_join(
  
  LDC_Table_Growth %>% select(Year, LDC_PC_GR), 
  
  Rich_Table %>% select(Year, Rich_PC_GR), 
  
  by = "Year"
) %>% 
  # we need to change it from rows in to collums so that I can plot it easily 
  pivot_longer(
    cols = c(LDC_PC_GR, Rich_PC_GR), 
    names_to = "Group",
    values_to = "Rate"
  ) %>% 
  #chaning the variable names so they look nice on the graph
  mutate(
    Group = case_when(
      Group == "LDC_PC_GR" ~ "All LDCs", 
      Group == "Rich_PC_GR" ~ "Rich World (Top 35)", 
      TRUE ~ Group
    )
  )%>%
  
  #I need to get rid of all the NA's which will happen in the initial data year
  
  filter(!is.na(Rate)) %>% 
  
  # now I need to do the indexing 
  group_by(Group) %>% 
  arrange(Year) %>% 
  mutate(
    #have to set rate -1 so that the initial year is 100 because i filtered out the initial row as it was NA 
    GDP_Index = 100 * cumprod(c(1, 1 + Rate[-1]))
  )

Plot_Index <- ggplot(GDP_Index_Table, aes(x = Year, y = GDP_Index, color = Group)) +
  geom_line(linewidth = 1.2) +
  
  # Reference line at 100 (Starting Line)
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey") +
  
  scale_color_manual(values = c("All LDCs" = "black", "Rich World (Top 35)" = "#619CFF")) +
  
  labs(
    title = "Real Income Growth (1990 = 100)",
    subtitle = "Cumulative Real GDP Per Capita: LDCs vs. Rich World",
    y = "GDP Index Value (Start = 100)",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(Plot_Index)

# I also need to look at non LDC countries to see if they are sustianing growth
head(Final_Table)

Non_LDC_Performance <- Final_Table %>% 
  #remove LDCs and get rid of NA's
  filter(LDC == 0) %>% 
  filter(!is.na(Growth_Rate_GDP) & !is.na(Growth_Rate_POP)) %>% 
  
  #caculate per capita growth rate
  mutate(Per_Capita_Growth =((1 + Growth_Rate_GDP) / (1 + Growth_Rate_POP)) -1) %>% 
  
  # group by country to look at growth rate of each country
  group_by(Country, Continent) %>% 
  summarise(
    #Using geometric mean
    Geo_Mean_Growth = prod(1 + Per_Capita_Growth, na.rm = TRUE)^(1 / n()) -1, 
    
    .groups = "drop"
  ) %>% 
  
  # filtering countries that are shrinking or stagnant 
  filter(Geo_Mean_Growth < 0.005)

# plot these economies 

Plot_Stagnation <- ggplot(Non_LDC_Performance, aes(x = reorder(Country, Geo_Mean_Growth), y = Geo_Mean_Growth, fill = Continent)) +
  geom_col() +
  coord_flip() + # Flip to make names readable
  
  # Add a red line at 0%
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  
  # Add labels with the exact percentage
  geom_text(aes(label = scales::percent(Geo_Mean_Growth, accuracy = 0.1)), 
            hjust = ifelse(Non_LDC_Performance$Geo_Mean_Growth > 0, -0.1, 1.1),
            size = 3.5) +
  
  scale_y_continuous(labels = scales::percent) +
  
  labs(
    title = "Non-LDCs with Stagnant or Negative Growth (1990-2022)",
    subtitle = "Geometric Mean (CAGR) of Per Capita Growth < 0.5%",
    x = NULL,
    y = "Average Annual Per Capita Growth (Geometric Mean)",
    fill = "Continent"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(color = "black", size = 10),
    legend.position = "bottom"
  )

print(Plot_Stagnation)

# -------------------------------------------------------------------------

#QUESTION 2 ANALYSIS

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

