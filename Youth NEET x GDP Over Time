#calculating the Youth NEET x GDP per capita and summing per continent per year  
p2 <- Final_Table %>%
  group_by(Year, Continent) %>%
  mutate(NEETGDP = YNEET * GDP_Per_Capita) %>%
  summarise(NEETGDP = sum(NEETGDP), .groups = "drop")

#plotting Youth NEET x GDP per capita and time for all continents 
ggplot(p2, aes(x = Year, y = NEETGDP, colour = Continent)) +
  geom_smooth(linewidth = 1) +
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
    title = 'Youth NEET x GDP Per Capita Over Time (Africa, Asia, Oceania)',
    x = 'Year',
    y = 'Youth NEET x GDP Per Capita'
  ) +
  theme_minimal()


#plotting individual graphs for each country 
continents <- unique(p2$Continent)

for(c in continents){
  p <- p2 %>%
    filter(Continent == c) %>%
    ggplot(aes(x = Year, y = NEETGDP, colour = Continent)) +
    geom_smooth(size = 0.75) +
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
     title = paste('Youth NEET x GDP Per Capita Over Time:', c),
     x = "Year",
     y = 'Youth NEET x GDP Per Capita'
     ) +
    theme_minimal()
  
  print(p)
}
