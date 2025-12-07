# ============================================================================
# UN SUSTAINABLE DEVELOPMENT GOALS ANALYSIS (1990-2020)
# Targets: Economic Growth & Youth NEET Reduction
# ============================================================================

library(tidyverse)
library(ggplot2)
library(scales)

# Set working directory to where your CSV files are located
# setwd("YOUR_PATH_HERE")

# ============================================================================
# 1. LOAD AND CLEAN DATA
# ============================================================================

# Load continents data
continents <- read_csv("Continents according to Our World in Data.csv")
continents_clean <- continents %>%
  select(Entity, Continent) %>%
  distinct()

# Load GDP data
gdp <- read_csv("gdppercapitaworldbank.csv")
gdp_clean <- gdp %>%
  rename(country = Entity, year = Year, gdp_per_capita = `GDP per capita, PPP (constant 2017 international $)`) %>%
  filter(year >= 1990, year <= 2020) %>%
  left_join(continents_clean, by = c("country" = "Entity")) %>%
  filter(Continent %in% c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")) %>%
  filter(!is.na(gdp_per_capita))

# Load NEET data
neet <- read_csv("youthnotineducationemploymenttraining.csv")
neet_clean <- neet %>%
  rename(country = Entity, year = Year, neet_rate = `Share of youth not in education, employment or training, total (% of youth population)`) %>%
  filter(year >= 1990, year <= 2020) %>%
  left_join(continents_clean, by = c("country" = "Entity")) %>%
  filter(Continent %in% c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")) %>%
  filter(!is.na(neet_rate))

# ============================================================================
# 2. CALCULATE CONTINENTAL AVERAGES
# ============================================================================

# GDP trends by continent
gdp_trends <- gdp_clean %>%
  group_by(Continent, year) %>%
  summarise(avg_gdp = mean(gdp_per_capita, na.rm = TRUE), .groups = "drop")

# NEET trends by continent
neet_trends <- neet_clean %>%
  group_by(Continent, year) %>%
  summarise(avg_neet = mean(neet_rate, na.rm = TRUE), .groups = "drop")

# ============================================================================
# 3. CALCULATE GROWTH RATES (1990-2020)
# ============================================================================

gdp_growth <- gdp_trends %>%
  group_by(Continent) %>%
  filter(year == min(year) | year == max(year)) %>%
  arrange(Continent, year) %>%
  summarise(
    first_year = first(year),
    last_year = last(year),
    first_gdp = first(avg_gdp),
    last_gdp = last(avg_gdp),
    years = last_year - first_year,
    annual_growth = ((last_gdp / first_gdp)^(1/years) - 1) * 100,
    .groups = "drop"
  )

# ============================================================================
# 4. CALCULATE NEET CHANGE (2015-2020)
# ============================================================================

neet_change <- neet_trends %>%
  filter(year %in% c(2015, 2020)) %>%
  group_by(Continent) %>%
  summarise(
    neet_2015 = avg_neet[year == 2015],
    neet_2020 = avg_neet[year == 2020],
    change = neet_2020 - neet_2015,
    .groups = "drop"
  ) %>%
  filter(!is.na(neet_2015) & !is.na(neet_2020))

# ============================================================================
# 5. PREPARE SCATTER PLOT DATA (2020)
# ============================================================================

scatter_data <- gdp_clean %>%
  filter(year == 2020) %>%
  left_join(
    neet_clean %>% filter(year == 2020) %>% select(country, neet_rate),
    by = "country"
  ) %>%
  filter(!is.na(neet_rate))

# ============================================================================
# 6. VISUALIZATION 1: GDP TRENDS (1990-2020)
# ============================================================================

plot1 <- ggplot(gdp_trends, aes(x = year, y = avg_gdp, color = Continent)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c(
    "Africa" = "#e74c3c",
    "Asia" = "#3498db",
    "Europe" = "#2ecc71",
    "North America" = "#f39c12",
    "South America" = "#9b59b6",
    "Oceania" = "#1abc9c"
  )) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Target 1: GDP Per Capita Trends by Continent (1990-2020)",
    subtitle = "Average GDP per capita (PPP, constant 2017 international $)",
    x = "Year",
    y = "GDP per Capita ($)",
    color = "Continent"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(plot1)
ggsave("1_gdp_trends.png", plot1, width = 12, height = 6, dpi = 300)

# ============================================================================
# 7. VISUALIZATION 2: ANNUAL GROWTH RATES
# ============================================================================

plot2 <- ggplot(gdp_growth, aes(x = reorder(Continent, annual_growth), y = annual_growth, fill = Continent)) +
  geom_col() +
  geom_hline(yintercept = 7, linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = paste0(round(annual_growth, 2), "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c(
    "Africa" = "#e74c3c",
    "Asia" = "#3498db",
    "Europe" = "#2ecc71",
    "North America" = "#f39c12",
    "South America" = "#9b59b6",
    "Oceania" = "#1abc9c"
  )) +
  labs(
    title = "Annual GDP Growth Rates by Continent (1990-2020)",
    subtitle = "Red dashed line indicates UN 7% target for LDCs",
    x = "Continent",
    y = "Average Annual Growth Rate (%)",
    fill = "Continent"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(plot2)
ggsave("2_growth_rates.png", plot2, width = 10, height = 6, dpi = 300)

# ============================================================================
# 8. VISUALIZATION 3: NEET TRENDS (1990-2020)
# ============================================================================

plot3 <- ggplot(neet_trends, aes(x = year, y = avg_neet, color = Continent)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c(
    "Africa" = "#e74c3c",
    "Asia" = "#3498db",
    "Europe" = "#2ecc71",
    "North America" = "#f39c12",
    "South America" = "#9b59b6",
    "Oceania" = "#1abc9c"
  )) +
  labs(
    title = "Target 2: Youth NEET Rates by Continent (1990-2020)",
    subtitle = "Share of youth not in education, employment or training (% of youth population)",
    x = "Year",
    y = "NEET Rate (%)",
    color = "Continent"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(plot3)
ggsave("3_neet_trends.png", plot3, width = 12, height = 6, dpi = 300)

# ============================================================================
# 9. VISUALIZATION 4: NEET CHANGE (2015 vs 2020)
# ============================================================================

plot4 <- ggplot(neet_change, aes(x = reorder(Continent, change), y = change, fill = change < 0)) +
  geom_col() +
  geom_text(aes(label = paste0(ifelse(change > 0, "+", ""), round(change, 2), "%")), 
            vjust = ifelse(neet_change$change > 0, -0.5, 1.5), size = 4, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                    labels = c("Improved (Decreased)", "Worsened (Increased)")) +
  labs(
    title = "NEET Rate Change: 2015 vs 2020",
    subtitle = "Negative values indicate improvement (reduction in NEET)",
    x = "Continent",
    y = "Change in NEET Rate (%)",
    fill = "Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(plot4)
ggsave("4_neet_change.png", plot4, width = 10, height = 6, dpi = 300)

# ============================================================================
# 10. VISUALIZATION 5: GDP vs NEET CORRELATION (2020)
# ============================================================================

plot5 <- ggplot(scatter_data, aes(x = gdp_per_capita, y = neet_rate, color = Continent)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 0.5) +
  scale_color_manual(values = c(
    "Africa" = "#e74c3c",
    "Asia" = "#3498db",
    "Europe" = "#2ecc71",
    "North America" = "#f39c12",
    "South America" = "#9b59b6",
    "Oceania" = "#1abc9c"
  )) +
  scale_x_continuous(labels = dollar_format()) +
  labs(
    title = "GDP vs NEET: Is There a Relationship? (2020)",
    subtitle = "Each point represents a country",
    x = "GDP per Capita ($)",
    y = "NEET Rate (%)",
    color = "Continent"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(plot5)
ggsave("5_gdp_vs_neet.png", plot5, width = 12, height = 7, dpi = 300)

# ============================================================================
# 11. PRINT SUMMARY TABLES
# ============================================================================

cat("\n========================================\n")
cat("GDP GROWTH RATES (1990-2020)\n")
cat("========================================\n")
print(gdp_growth %>% 
        select(Continent, annual_growth) %>%
        arrange(desc(annual_growth)))

cat("\n========================================\n")
cat("NEET CHANGE (2015-2020)\n")
cat("========================================\n")
print(neet_change %>% 
        arrange(change))

cat("\n========================================\n")
cat("CORRELATION: GDP vs NEET (2020)\n")
cat("========================================\n")
correlation <- cor(scatter_data$gdp_per_capita, scatter_data$neet_rate, use = "complete.obs")
cat("Correlation coefficient:", round(correlation, 3), "\n")
if (correlation < 0) {
  cat("Interpretation: Negative correlation - higher GDP tends to associate with lower NEET rates\n")
} else {
  cat("Interpretation: Positive correlation - higher GDP tends to associate with higher NEET rates\n")
}

# ============================================================================
# 12. EXPORT SUMMARY DATA
# ============================================================================

write_csv(gdp_growth, "summary_gdp_growth.csv")
write_csv(neet_change, "summary_neet_change.csv")
write_csv(scatter_data, "data_gdp_neet_2020.csv")

cat("\n========================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("========================================\n")
cat("Generated files:\n")
cat("  - 1_gdp_trends.png\n")
cat("  - 2_growth_rates.png\n")
cat("  - 3_neet_trends.png\n")
cat("  - 4_neet_change.png\n")
cat("  - 5_gdp_vs_neet.png\n")
cat("  - summary_gdp_growth.csv\n")
cat("  - summary_neet_change.csv\n")
cat("  - data_gdp_neet_2020.csv\n")
cat("========================================\n")

# ============================================================================
# YOUTH NEET RATES BY CONTINENT - BAR CHART (1990-2022)
# ============================================================================

library(tidyverse)
library(ggplot2)

# Set working directory to where your CSV files are located
# setwd("YOUR_PATH_HERE")

# ============================================================================
# 1. LOAD AND CLEAN DATA
# ============================================================================

# Load continents data
continents <- read_csv("Continents according to Our World in Data.csv")
continents_clean <- continents %>%
  select(Entity, Continent) %>%
  distinct()

# Load NEET data
neet <- read_csv("youthnotineducationemploymenttraining.csv")
neet_clean <- neet %>%
  rename(
    country = Entity, 
    year = Year, 
    neet_rate = `Share of youth not in education, employment or training, total (% of youth population)`
  ) %>%
  filter(year >= 1990, year <= 2022) %>%
  left_join(continents_clean, by = c("country" = "Entity")) %>%
  filter(Continent %in% c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")) %>%
  filter(!is.na(neet_rate))

# ============================================================================
# 2. CALCULATE AVERAGE NEET RATES BY CONTINENT AND YEAR
# ============================================================================

neet_by_continent_year <- neet_clean %>%
  group_by(Continent, year) %>%
  summarise(avg_neet = mean(neet_rate, na.rm = TRUE), .groups = "drop")

# ============================================================================
# 3. CREATE BAR CHART
# ============================================================================

# Define continent colors
continent_colors <- c(
  "Africa" = "#e74c3c",
  "Asia" = "#3498db",
  "Europe" = "#2ecc71",
  "North America" = "#f39c12",
  "South America" = "#9b59b6",
  "Oceania" = "#1abc9c"
)

# Create the bar chart
plot_neet_bars <- ggplot(neet_by_continent_year, 
                         aes(x = as.factor(year), y = avg_neet, fill = Continent)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_manual(values = continent_colors) +
  labs(
    title = "Youth NEET Rates by Continent (1990-2022)",
    subtitle = "Average share of youth not in education, employment or training",
    x = "Year",
    y = "NEET Rate (%)",
    fill = "Continent"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "right",
    panel.grid.major.x = element_blank()
  )

# Display the plot
print(plot_neet_bars)

# Save the plot
ggsave("neet_rates_bar_chart_1990_2022.png", 
       plot_neet_bars, 
       width = 16, 
       height = 7, 
       dpi = 300)

# ============================================================================
# 4. ALTERNATIVE: FACETED BAR CHART (One chart per continent)
# ============================================================================

plot_neet_faceted <- ggplot(neet_by_continent_year, 
                            aes(x = as.factor(year), y = avg_neet, fill = Continent)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Continent, nrow = 2) +
  scale_fill_manual(values = continent_colors) +
  labs(
    title = "Youth NEET Rates by Continent (1990-2022)",
    subtitle = "Faceted view - each panel shows one continent",
    x = "Year",
    y = "NEET Rate (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major.x = element_blank()
  )

print(plot_neet_faceted)

ggsave("neet_rates_faceted_1990_2022.png", 
       plot_neet_faceted, 
       width = 14, 
       height = 8, 
       dpi = 300)

# ============================================================================
# 5. ALTERNATIVE: GROUPED BAR CHART (Select Years Only)
# ============================================================================

# Select specific years
selected_years <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2022)

neet_selected_years <- neet_by_continent_year %>%
  filter(year %in% selected_years)

plot_neet_selected <- ggplot(neet_selected_years, 
                             aes(x = as.factor(year), y = avg_neet, fill = Continent)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_manual(values = continent_colors) +
  labs(
    title = "Youth NEET Rates by Continent - Key Years (1990-2022)",
    subtitle = "Average share of youth not in education, employment or training",
    x = "Year",
    y = "NEET Rate (%)",
    fill = "Continent"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    legend.position = "right",
    panel.grid.major.x = element_blank()
  )

print(plot_neet_selected)

ggsave("neet_rates_selected_years.png", 
       plot_neet_selected, 
       width = 12, 
       height = 6, 
       dpi = 300)

# ============================================================================
# 6. PRINT SUMMARY STATISTICS
# ============================================================================

cat("\n========================================\n")
cat("NEET RATES SUMMARY (1990-2022)\n")
cat("========================================\n\n")

summary_stats <- neet_by_continent_year %>%
  group_by(Continent) %>%
  summarise(
    min_neet = min(avg_neet),
    max_neet = max(avg_neet),
    avg_neet = mean(avg_neet),
    latest_year = max(year),
    latest_neet = avg_neet[year == max(year)]
  )

print(summary_stats)

# Export summary data
write_csv(neet_by_continent_year, "neet_rates_by_continent_1990_2022.csv")
write_csv(summary_stats, "neet_summary_statistics.csv")

cat("\n========================================\n")
cat("CHARTS CREATED SUCCESSFULLY!\n")
cat("========================================\n")
cat("Generated files:\n")
cat("  - neet_rates_bar_chart_1990_2022.png (all years, grouped)\n")
cat("  - neet_rates_faceted_1990_2022.png (separate panel per continent)\n")
cat("  - neet_rates_selected_years.png (key years only)\n")
cat("  - neet_rates_by_continent_1990_2022.csv (data export)\n")
cat("  - neet_summary_statistics.csv (summary stats)\n")
cat("========================================\n")
