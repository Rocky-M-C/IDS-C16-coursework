# UN SUSTAINABLE DEVELOPMENT GOALS ANALYSIS (1990-2022)
library(tidyverse)
# LOAD DATA
continents <- read_csv("Continents according to Our World in Data.csv") %>%
  select(Entity, Continent) %>% distinct()

gdp <- read_csv("gdppercapitaworldbank.csv") %>%
  rename(country = Entity, year = Year, gdp = `GDP per capita, PPP (constant 2017 international $)`) %>%
  filter(year >= 1990, year <= 2020) %>%
  left_join(continents, by = c("country" = "Entity")) %>%
  filter(Continent %in% c("Africa", "Asia", "Europe", "North America", "South America", "Oceania"), !is.na(gdp))

neet <- read_csv("youthnotineducationemploymenttraining.csv") %>%
  rename(country = Entity, year = Year, neet = `Share of youth not in education, employment or training, total (% of youth population)`) %>%
  filter(year >= 1990, year <= 2022) %>%
  left_join(continents, by = c("country" = "Entity")) %>%
  filter(Continent %in% c("Africa", "Asia", "Europe", "North America", "South America", "Oceania"), !is.na(neet))

# CONTINENT COLORS
cols <- c(Africa = "#e74c3c", Asia = "#3498db", Europe = "#2ecc71", 
          `North America` = "#f39c12", `South America` = "#9b59b6", Oceania = "#1abc9c")

# 1. GDP TRENDS
gdp %>%
  group_by(Continent, year) %>%
  summarise(avg_gdp = mean(gdp), .groups = "drop") %>%
  ggplot(aes(year, avg_gdp, color = Continent)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "GDP Per Capita Trends by Continent (1990-2020)", x = "Year", y = "GDP per Capita ($)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))
ggsave("1_gdp_trends.png", width = 12, height = 6, dpi = 300)

# 2. GDP GROWTH RATES
gdp %>%
  group_by(Continent, year) %>%
  summarise(avg_gdp = mean(gdp), .groups = "drop") %>%
  group_by(Continent) %>%
  filter(year %in% c(min(year), max(year))) %>%
  summarise(growth = ((last(avg_gdp) / first(avg_gdp))^(1/(last(year) - first(year))) - 1) * 100, .groups = "drop") %>%
  ggplot(aes(reorder(Continent, growth), growth, fill = Continent)) +
  geom_col() +
  geom_hline(yintercept = 7, linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = paste0(round(growth, 2), "%")), vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = cols) +
  labs(title = "Annual GDP Growth Rates by Continent (1990-2020)",
       x = "Continent", y = "Growth Rate (%)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14), legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("2_growth_rates.png", width = 10, height = 6, dpi = 300)

# 3. NEET TRENDS
neet %>%
  group_by(Continent, year) %>%
  summarise(avg_neet = mean(neet), .groups = "drop") %>%
  ggplot(aes(year, avg_neet, color = Continent)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = cols) +
  labs(title = "Youth NEET Rates by Continent (1990-2022)",
       x = "Year", y = "NEET Rate (%)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))
ggsave("3_neet_trends.png", width = 12, height = 6, dpi = 300)

# 4. NEET CHANGE (2015-2020)
neet %>%
  group_by(Continent, year) %>%
  summarise(avg_neet = mean(neet), .groups = "drop") %>%
  filter(year %in% c(2015, 2020)) %>%
  group_by(Continent) %>%
  summarise(change = last(avg_neet) - first(avg_neet), .groups = "drop") %>%
  ggplot(aes(reorder(Continent, change), change, fill = change < 0)) +
  geom_col() +
  geom_text(aes(label = paste0(ifelse(change > 0, "+", ""), round(change, 2), "%")), 
            vjust = ifelse(.$change > 0, -0.5, 1.5), size = 4, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c")) +
  labs(title = "NEET Rate Change: 2015 vs 2020", x = "Continent", y = "Change (%)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14), legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("4_neet_change.png", width = 10, height = 6, dpi = 300)

# 5. GDP VS NEET (2020)
gdp %>%
  filter(year == 2020) %>%
  left_join(neet %>% filter(year == 2020) %>% select(country, neet), by = "country") %>%
  filter(!is.na(neet)) %>%
  ggplot(aes(gdp, neet, color = Continent)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 0.5) +
  scale_color_manual(values = cols) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(title = "GDP vs NEET (2020)", x = "GDP per Capita ($)", y = "NEET Rate (%)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))
ggsave("5_gdp_vs_neet.png", width = 12, height = 7, dpi = 300)

# 6. NEET BAR CHART - ALL YEARS
neet %>%
  group_by(Continent, year) %>%
  summarise(avg_neet = mean(neet), .groups = "drop") %>%
  ggplot(aes(as.factor(year), avg_neet, fill = Continent)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_manual(values = cols) +
  labs(title = "Youth NEET Rates by Continent (1990-2022)", x = "Year", y = "NEET Rate (%)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank())
ggsave("6_neet_bars_all.png", width = 16, height = 7, dpi = 300)

# 7. NEET BAR CHART - FACETED
neet %>%
  group_by(Continent, year) %>%
  summarise(avg_neet = mean(neet), .groups = "drop") %>%
  ggplot(aes(as.factor(year), avg_neet, fill = Continent)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Continent, nrow = 2) +
  scale_fill_manual(values = cols) +
  labs(title = "Youth NEET Rates by Continent (1990-2022)", x = "Year", y = "NEET Rate (%)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
        strip.text = element_text(face = "bold", size = 11),
        panel.grid.major.x = element_blank())
ggsave("7_neet_faceted.png", width = 14, height = 8, dpi = 300)

# 8. NEET BAR CHART - SELECTED YEARS
neet %>%
  group_by(Continent, year) %>%
  summarise(avg_neet = mean(neet), .groups = "drop") %>%
  filter(year %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2022)) %>%
  ggplot(aes(as.factor(year), avg_neet, fill = Continent)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_manual(values = cols) +
  labs(title = "Youth NEET Rates - Key Years (1990-2022)", x = "Year", y = "NEET Rate (%)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        panel.grid.major.x = element_blank())
ggsave("8_neet_selected.png", width = 12, height = 6, dpi = 300)


##############################################################################################
library(tidyverse)

# Load continents
continents <- read_csv("Continents according to Our World in Data.csv") %>%
  select(Entity, Continent) %>%
  distinct()

# Load and clean GDP data
gdp_clean <- read_csv("gdppercapitaworldbank.csv") %>%
  rename(country = Entity, year = Year, gdp_per_capita = `GDP per capita, PPP (constant 2017 international $)`) %>%
  filter(year >= 1990, year <= 2020) %>%
  left_join(continents, by = c("country" = "Entity")) %>%
  filter(Continent %in% c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")) %>%
  filter(!is.na(gdp_per_capita))

# Load and clean NEET data
neet_clean <- read_csv("youthnotineducationemploymenttraining.csv") %>%
  rename(country = Entity, year = Year, neet_rate = `Share of youth not in education, employment or training, total (% of youth population)`) %>%
  filter(year >= 1990, year <= 2022) %>%
  left_join(continents, by = c("country" = "Entity")) %>%
  filter(Continent %in% c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")) %>%
  filter(!is.na(neet_rate))

# 2. CALCULATE AVERAGES

# GDP by continent and year
gdp_trends <- gdp_clean %>%
  group_by(Continent, year) %>%
  summarise(avg_gdp = mean(gdp_per_capita), .groups = "drop")

# NEET by continent and year
neet_trends <- neet_clean %>%
  group_by(Continent, year) %>%
  summarise(avg_neet = mean(neet_rate), .groups = "drop")

# GDP growth rates
gdp_growth <- gdp_trends %>%
  group_by(Continent) %>%
  filter(year == min(year) | year == max(year)) %>%
  summarise(first_gdp = first(avg_gdp),last_gdp = last(avg_gdp),years = last(year) - first(year),annual_growth = ((last_gdp / first_gdp)^(1/years) - 1) * 100,.groups = "drop")

# NEET change 2015-2020
neet_change <- neet_trends %>%
  filter(year %in% c(2015, 2020)) %>%
  group_by(Continent) %>%
  summarise(neet_2015 = avg_neet[year == 2015],neet_2020 = avg_neet[year == 2020],change = neet_2020 - neet_2015,.groups = "drop")

# GDP vs NEET in 2020
scatter_data <- gdp_clean %>%
  filter(year == 2020) %>%
  left_join(neet_clean %>% filter(year == 2020) %>% select(country, neet_rate), 
            by = "country") %>%
  filter(!is.na(neet_rate))

# 3. DEFINE COLORS

continent_colors <- c(
  "Africa" = "#e74c3c",
  "Asia" = "#3498db",
  "Europe" = "#2ecc71",
  "North America" = "#f39c12",
  "South America" = "#9b59b6",
  "Oceania" = "#1abc9c"
)

# 4. VISUALIZATION 1: GDP TRENDS

ggplot(gdp_trends, aes(x = year, y = avg_gdp, color = Continent)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = continent_colors) +
  labs(title = "GDP Per Capita Trends by Continent (1990-2020)",
       x = "Year", y = "GDP per Capita ($)") +
  theme_minimal()

ggsave("1_gdp_trends.png", width = 12, height = 6)

# 5. VISUALIZATION 2: GROWTH RATES

ggplot(gdp_growth, aes(x = reorder(Continent, annual_growth), y = annual_growth, fill = Continent)) +
  geom_col() +
  geom_hline(yintercept = 7, linetype = "dashed", color = "red") +
  scale_fill_manual(values = continent_colors) +
  labs(title = "Annual GDP Growth Rates (1990-2020)",
       x = "Continent", y = "Growth Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("2_growth_rates.png", width = 10, height = 6)

# 6. VISUALIZATION 3: NEET TRENDS

ggplot(neet_trends, aes(x = year, y = avg_neet, color = Continent)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = continent_colors) +
  labs(title = "Youth NEET Rates by Continent (1990-2022)",
       x = "Year", y = "NEET Rate (%)") +
  theme_minimal()

ggsave("3_neet_trends.png", width = 12, height = 6)

# 7. VISUALIZATION 4: NEET CHANGE

ggplot(neet_change, aes(x = reorder(Continent, change), y = change, fill = change < 0)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c")) +
  labs(title = "NEET Rate Change: 2015 vs 2020",
       x = "Continent", y = "Change in NEET (%)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("4_neet_change.png", width = 10, height = 6)

# 8. VISUALIZATION 5: GDP vs NEET

ggplot(scatter_data, aes(x = gdp_per_capita, y = neet_rate, color = Continent)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(values = continent_colors) +
  labs(title = "GDP vs NEET (2020)",
       x = "GDP per Capita ($)", y = "NEET Rate (%)") +
  theme_minimal()

ggsave("5_gdp_vs_neet.png", width = 12, height = 7)

# 9. VISUALIZATION 6: NEET BAR CHART (ALL YEARS)

ggplot(neet_trends, aes(x = as.factor(year), y = avg_neet, fill = Continent)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = continent_colors) +
  labs(title = "Youth NEET Rates by Continent (1990-2022)",
       x = "Year", y = "NEET Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("6_neet_bar_all_years.png", width = 16, height = 7)

# 10. VISUALIZATION 7: NEET BAR CHART (SELECTED YEARS)

neet_trends %>%
  filter(year %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2022)) %>%
  ggplot(aes(x = as.factor(year), y = avg_neet, fill = Continent)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = continent_colors) +
  labs(title = "Youth NEET Rates - Key Years (1990-2022)",
       x = "Year", y = "NEET Rate (%)") +
  theme_minimal()

ggsave("7_neet_bar_selected.png", width = 12, height = 6)

# 11. VISUALIZATION 8: NEET FACETED

ggplot(neet_trends, aes(x = as.factor(year), y = avg_neet, fill = Continent)) +
  geom_col() +
  facet_wrap(~Continent, nrow = 2) +
  scale_fill_manual(values = continent_colors) +
  labs(title = "Youth NEET Rates by Continent (1990-2022)",
       x = "Year", y = "NEET Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        legend.position = "none")

ggsave("8_neet_faceted.png", width = 14, height = 8)

# 12. PRINT SUMMARY

print("GDP Growth Rates:")
print(gdp_growth)

print("NEET Change 2015-2020:")
print(neet_change)

print("Correlation GDP vs NEET:")
print(cor(scatter_data$gdp_per_capita, scatter_data$neet_rate))

