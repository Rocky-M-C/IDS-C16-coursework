#here I am sourcing the other documents from my lovely collegues
library(tidyverse)

source("C:/Users/rohan/OneDrive/Desktop/IDS c16/IDS-C16-coursework/Full Data Table")
source("C:/Users/rohan/OneDrive/Desktop/IDS c16/IDS-C16-coursework/Adding LDC")

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


