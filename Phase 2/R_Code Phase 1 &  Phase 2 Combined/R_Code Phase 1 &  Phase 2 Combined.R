# ==================== PHASE 1: DATA PREPARATION ====================

# Load libraries
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(corrplot)

# Load datasets
unicef <- read.csv("UNICEF_malnutrition.csv")
faostat <- read.csv("FAOSTAT_data_en_10-12-2025.csv")

# Data cleaning and merging
yearly_indicators <- c(
  "Dietary energy supply used in the estimation of the prevalence of undernourishment (kcal/cap/day)",
  "Prevalence of anemia among women of reproductive age (15-49 years) (percent)",
  "Prevalence of obesity in the adult population (18 years and older) (percent)",
  "Percentage of children under 5 years of age who are stunted (modelled estimates) (percent)",
  "Political stability and absence of violence/terrorism (index)",
  "Percentage of population using safely managed drinking water services (percent)",
  "Percentage of population using at least basic sanitation services (percent)"
)

faostat_yearly <- faostat %>%
  filter(Item %in% yearly_indicators & !grepl("-", Year)) %>%
  mutate(Year = as.numeric(Year)) %>%
  select(Year, Item, Value, Unit) %>%
  drop_na(Year)

faostat_wide <- faostat_yearly %>%
  pivot_wider(
    names_from = Item,
    values_from = Value,
    values_fn = mean
  )

names(faostat_wide) <- gsub(" \\(percent\\)", "", names(faostat_wide))
names(faostat_wide) <- gsub(" \\(modelled estimates\\)", "", names(faostat_wide))
names(faostat_wide) <- gsub("Percentage of ", "", names(faostat_wide))
names(faostat_wide) <- gsub("Prevalence of ", "", names(faostat_wide))
names(faostat_wide) <- make.names(names(faostat_wide))

merged_data <- unicef %>%
  inner_join(faostat_wide, by = "Year")

# Clean and combine data
merged_data_clean <- merged_data %>%
  select(Country, Year, Malnutrition_Percent, 
         Dietary.energy.supply.used.in.the.estimation.of.the.prevalence.of.undernourishment..kcal.cap.day.,
         Political.stability.and.absence.of.violence.terrorism..index.,
         population.using.safely.managed.drinking.water.services,
         population.using.at.least.basic.sanitation.services,
         children.under.5.years.of.age.who.are.stunted,
         obesity.in.the.adult.population..18.years.and.older.,
         anemia.among.women.of.reproductive.age..15.49.years.)

colnames(merged_data_clean) <- c("Country", "Year", "Malnutrition_Percent", "Dietary_Energy_Supply", 
                                 "Political_Stability", "Safe_Water_Access", "Basic_Sanitation",
                                 "Child_Stunting", "Adult_Obesity", "Women_Anemia")

merged_data_combined <- merged_data_clean %>%
  group_by(Year) %>%
  summarise(
    Country = first(Country),
    Malnutrition_Percent = first(Malnutrition_Percent),
    Dietary_Energy_Supply = first(na.omit(Dietary_Energy_Supply)),
    Political_Stability = first(na.omit(Political_Stability)),
    Safe_Water_Access = first(na.omit(Safe_Water_Access)),
    Basic_Sanitation = first(na.omit(Basic_Sanitation)),
    Child_Stunting = first(na.omit(Child_Stunting)),
    Adult_Obesity = first(na.omit(Adult_Obesity)),
    Women_Anemia = first(na.omit(Women_Anemia))
  ) %>%
  ungroup()

final_data <- merged_data_combined %>%
  filter(Year %in% c(2000, 2005, 2010, 2015, 2020, 2022, 2024)) %>%
  arrange(Year)

cat("=== PHASE 1 COMPLETED: Data Cleaning & Merging ===\n")
print("Final cleaned data:")
print(final_data)


# ==================== PHASE 2: EDA & VISUALIZATION ====================

# Create directory for outputs
if(!dir.exists("analysis_outputs")) {
  dir.create("analysis_outputs")
}

# 1. Malnutrition Trend Plot
p1 <- ggplot(final_data, aes(x = Year, y = Malnutrition_Percent)) +
  geom_line(color = "red", size = 2) +
  geom_point(size = 4, color = "red") +
  geom_text(aes(label = paste0(Malnutrition_Percent, "%")), vjust = -1, size = 4, fontface = "bold") +
  labs(title = "DECLINE IN MALNUTRITION IN PAKISTAN (2000-2024)",
       subtitle = "Malnutrition percentage shows consistent decrease over 24 years",
       x = "Year", y = "Malnutrition Percentage (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Save plot
ggsave("analysis_outputs/01_malnutrition_trend.png", p1, width = 10, height = 6, dpi = 300)

# 2. Multiple Indicators Trend
trend_data <- final_data %>%
  select(Year, Malnutrition_Percent, Child_Stunting, Women_Anemia, Adult_Obesity) %>%
  pivot_longer(cols = -Year, names_to = "Indicator", values_to = "Value")

trend_data <- trend_data %>%
  mutate(Indicator = case_when(
    Indicator == "Malnutrition_Percent" ~ "Malnutrition",
    Indicator == "Child_Stunting" ~ "Child Stunting",
    Indicator == "Women_Anemia" ~ "Women Anemia", 
    Indicator == "Adult_Obesity" ~ "Adult Obesity",
    TRUE ~ Indicator
  ))

p2 <- ggplot(trend_data, aes(x = Year, y = Value, color = Indicator, group = Indicator)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  facet_wrap(~Indicator, scales = "free_y", ncol = 2) +
  labs(title = "TRENDS IN NUTRITION & HEALTH INDICATORS - PAKISTAN (2000-2024)",
       y = "Percentage (%)", x = "Year") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("analysis_outputs/02_multiple_indicators.png", p2, width = 12, height = 8, dpi = 300)

# 3. Correlation Analysis
cor_pairwise <- cor(final_data %>% 
                      select(Malnutrition_Percent, Child_Stunting, Women_Anemia, 
                             Adult_Obesity, Safe_Water_Access, Basic_Sanitation),
                    use = "pairwise.complete.obs")

png("analysis_outputs/03_correlation_matrix.png", width = 8, height = 6, units = "in", res = 300)
corrplot(cor_pairwise, method = "color", type = "upper",
         title = "CORRELATION MATRIX: Nutrition Indicators\nPakistan (2000-2024)",
         mar = c(0, 0, 2, 0), tl.col = "black", tl.srt = 45)
dev.off()

# 4. Scatter Plots
# Malnutrition vs Child Stunting
p3 <- ggplot(final_data, aes(x = Child_Stunting, y = Malnutrition_Percent)) +
  geom_point(size = 5, color = "blue", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_text(aes(label = Year), vjust = -0.8, size = 4, fontface = "bold") +
  labs(title = "MALNUTRITION vs CHILD STUNTING",
       subtitle = "Strong positive correlation observed",
       x = "Child Stunting (%)", y = "Malnutrition (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Malnutrition vs Women Anemia
p4 <- ggplot(final_data, aes(x = Women_Anemia, y = Malnutrition_Percent)) +
  geom_point(size = 5, color = "purple", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "brown", linetype = "dashed") +
  geom_text(aes(label = Year), vjust = -0.8, size = 4, fontface = "bold") +
  labs(title = "MALNUTRITION vs WOMEN ANEMIA",
       subtitle = "Positive correlation between malnutrition and anemia",
       x = "Women Anemia (%)", y = "Malnutrition (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Combine scatter plots
p_combined <- grid.arrange(p3, p4, ncol = 2)
ggsave("analysis_outputs/04_scatter_plots.png", p_combined, width = 14, height = 6, dpi = 300)

# 5. Water & Sanitation vs Malnutrition
ws_data <- final_data %>%
  select(Year, Malnutrition_Percent, Safe_Water_Access, Basic_Sanitation) %>%
  pivot_longer(cols = c(Safe_Water_Access, Basic_Sanitation), 
               names_to = "Service", values_to = "Coverage")

p5 <- ggplot(ws_data, aes(x = Year)) +
  geom_line(aes(y = Malnutrition_Percent, color = "Malnutrition"), size = 2) +
  geom_line(aes(y = Coverage, color = Service), size = 1.5, linetype = "dashed", alpha = 0.8) +
  scale_color_manual(values = c("Malnutrition" = "red", 
                                "Safe_Water_Access" = "blue", 
                                "Basic_Sanitation" = "green"),
                     labels = c("Malnutrition", "Safe Water Access", "Basic Sanitation")) +
  labs(title = "MALNUTRITION vs WATER & SANITATION SERVICES",
       subtitle = "As water and sanitation improve, malnutrition decreases",
       y = "Percentage (%)", x = "Year", color = "Indicator") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom")

ggsave("analysis_outputs/05_water_sanitation_trend.png", p5, width = 10, height = 6, dpi = 300)

cat("=== PHASE 2 COMPLETED: EDA & Visualization ===\n")
cat("All graphs saved in 'analysis_outputs' folder:\n")
cat("✅ 01_malnutrition_trend.png\n")
cat("✅ 02_multiple_indicators.png\n") 
cat("✅ 03_correlation_matrix.png\n")
cat("✅ 04_scatter_plots.png\n")
cat("✅ 05_water_sanitation_trend.png\n")



