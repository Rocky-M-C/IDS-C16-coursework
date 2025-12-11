#calculating the population of each continent by year
continents_pop <- Final_Table %>%
  group_by(Continent, Year) %>%
  summarise(Conitnent_pop = sum(Population), .groups = 'drop') 

#joinig continent population to main dataset
Final_Table <- Final_Table %>%
  left_join(continents_pop, by = c('Continent', 'Year')) %>%
  mutate(NEET_weighted = YNEET * Population / Continent_pop)

view(Final_Table)

#sum the weighted NEET
p <- Final_Table %>%
  group_by(Year, Continent) %>%
  summarise(NEET_weighted = sum(NEET_weighted), .groups = "drop")

library(ggplot2)

#plot the graph between weighted NEET and year
ggplot(p, aes(x = Year, y = NEET_weighted, colour = Continent)) +
  geom_smooth(se = FALSE, linewidth = 1) +
  scale_colour_manual(
    values = c(
      "Africa" = "red",
      "Asia" = "gold",
      "Oceania" = "blue",
      "Europe" = "darkgreen",
      "North America" = "turquoise",
      "South America" = "magenta",
      "NA" = "grey40"
      )
  ) +
  labs(
    title = 'Youth NEET Weighted by Population Over Time (Africa, Asia, Oceania)',
    x = 'Year',
    y = 'Youth NEET Weighted by Population'
  ) +
  theme_minimal()


#plot the graph for Asia, Africa and Oceania
plot_1 <- p %>%
  filter(Continent %in% c('Africa', 'Asia', 'Oceania')) %>%
  ggplot(aes(x = Year, y = NEET_weighted, colour = Continent)) +
  geom_smooth(se = FALSE, linewidth = 1) +
  scale_colour_manual(
    values = c(
      "Africa" = "red",
      "Asia" = "gold",
      "Oceania" = "blue"
    )
  ) +
  labs(
    title = 'Youth NEET Weighted by Population Over Time (Africa, Asia, Oceania)',
    x = 'Year',
    y = 'Youth NEET Weighted by Population'
  ) +
  theme_minimal()
print(plot_1)


#plot the graph for Europe, North America, South America 
plot_2 <- p %>%
  filter(Continent %in% c('Europe', 'North America', 'South America')) %>%
  ggplot(aes(x = Year, y = NEET_weighted, colour = Continent)) +
  geom_smooth(se = FALSE, linewidth = 1) +
  scale_colour_manual(
    values = c(
      "Europe" = "darkgreen",
      "North America" = "turquoise",
      "South America" = "magenta" 
    )
  ) +
  labs(
    title = 'Youth NEET Weighted by Population Over Time (North America, South America, Europe)',
    x = 'Year',
    y = 'Youth NEET Weighted by Population'
  ) +
  theme_minimal()

print(plot_2)

#filter data from 2020 onwards and plot a graph 
date_filtered <- Final_Table %>%
  filter(Year >= 2020)


plot_3 <- date_filtered %>%
  filter(Continent %in% c('Africa', 'Asia', 'Oceania')) %>%
  ggplot(aes(x = Year, y = NEET_weighted, colour = Continent)) +
  geom_smooth(se = FALSE, linewidth = 1) +
  scale_colour_manual(
    values = c(
      "Africa" = "red",
      "Asia" = "gold",
      "Oceania" = "blue"
    )
  ) +
  labs(
    title = 'Youth NEET Weighted by Population Over Time (Africa, Asia, Oceania)',
    x = 'Year',
    y = 'Youth NEET Weighted by Population'
  ) +
  theme_minimal()
print(plot_3)

plot_4 <- date_filtered %>%
  filter(Continent %in% c('Europe', 'North America', 'South America')) %>%
  ggplot(aes(x = Year, y = NEET_weighted, colour = Continent)) +
  geom_smooth(se = FALSE, linewidth = 1) +
  scale_colour_manual(
    values = c(
      "Europe" = "darkgreen",
      "North America" = "turquoise",
      "South America" = "magenta" 
    )
  ) +
  labs(
    title = 'Youth NEET Weighted by Population Over Time (North America, South America, Europe)',
    x = 'Year',
    y = 'Youth NEET Weighted by Population'
  ) +
  theme_minimal()

print(plot_4)
