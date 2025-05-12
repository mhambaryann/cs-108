# Install packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("broom")
install.packages("scales")
install.packages("stargazer")

# Load libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(broom)
library(scales)
library(stargazer)

# Function to reshape WB-style data
reshape_wb_data <- function(df) {
  df %>%
    select(-`Country Code`, -`Indicator Name`) %>%
    pivot_longer(
      cols = -`Country Name`,
      names_to = "Year",
      values_to = "Value",
      values_drop_na = FALSE
    ) %>%
    mutate(Year = as.numeric(Year))
}

# Load and reshape datasets
# GDP Growth
gdp_armenia <- read_csv("gdp_growth.csv", show_col_types = FALSE) %>% 
  reshape_wb_data() %>% 
  rename(GDP_Growth = Value) %>% 
  filter(`Country Name` == "Armenia") %>%
  select(-`Country Name`)

# Fertility Rate
fertility_armenia <- read_csv("fertility_rate.csv", show_col_types = FALSE) %>%
  reshape_wb_data() %>%
  rename(Fertility_Rate = Value) %>%
  filter(`Country Name` == "Armenia") %>%
  select(-`Country Name`) %>%
  arrange(Year) %>%
  mutate(Fertility_Rate_Lag20 = lag(Fertility_Rate, n = 20))

# Labor Force Participation
lfpr_armenia <- read_csv("labor_force_participation.csv", show_col_types = FALSE) %>%
  reshape_wb_data() %>%
  rename(LFPR = Value) %>%
  filter(`Country Name` == "Armenia") %>%
  select(-`Country Name`)

# Dependency Ratio
dependency_armenia <- read_csv("age_dependency_ratio.csv", show_col_types = FALSE) %>%
  reshape_wb_data() %>%
  rename(Dependency_Ratio = Value) %>%
  filter(`Country Name` == "Armenia") %>%
  select(-`Country Name`)

# Total Labor Force 
labor_force_armenia <- read_csv("total_labor_force.csv", show_col_types = FALSE) %>%
  reshape_wb_data() %>%
  rename(Labor_Force = Value) %>%
  filter(`Country Name` == "Armenia") %>%
  select(-`Country Name`)

# Merge datasets
full_data <- gdp_armenia %>%
  left_join(fertility_armenia, by = "Year") %>%
  left_join(lfpr_armenia, by = "Year") %>%
  left_join(dependency_armenia, by = "Year") %>%
  left_join(labor_force_armenia, by = "Year") %>%
  filter(Year >= 1990 & Year <= 2023) %>%
  drop_na(GDP_Growth, Fertility_Rate_Lag20)

# Fit regression model
model <- lm(GDP_Growth ~ Fertility_Rate_Lag20, data = full_data)

# Create table
stargazer(model, type = "text", 
          title = "Regression Results",
          dep.var.labels = "GDP Growth",
          covariate.labels = "Fertility Rate (Lag 20)",
          omit.stat = c("f", "ser"), # optional: remove F-stat and residual std error
          digits = 3,
          ci = TRUE, # add confidence intervals
          single.row = TRUE)

# Predict GDP Growth for 2024–2043 using 20-year lagged fertility
future_fertility <- fertility_armenia %>%
  filter(Year >= 2004 & Year <= 2023) %>%
  transmute(Year = Year + 20, Fertility_Rate_Lag20 = Fertility_Rate)

# Predict GDP with confidence intervals
predictions <- predict(model, newdata = future_fertility, interval = "confidence", level = 0.95)

# Bind predictions to the data
future_data <- future_fertility %>%
  mutate(
    GDP_Growth = predictions[, "fit"],
    Lower_CI = predictions[, "lwr"],
    Upper_CI = predictions[, "upr"],
    Data_Type = "Predicted"
  )

# Final combined dataset
actual_data <- full_data %>%
  mutate(Data_Type = "Actual") %>%
  select(Year, GDP_Growth, Fertility_Rate_Lag20, LFPR, Dependency_Ratio, Labor_Force, Data_Type)

combined_data <- bind_rows(actual_data, future_data)

# Summary Statistics
combined_data %>%
  filter(Data_Type == "Actual") %>%
  select(Fertility_Rate_Lag20, GDP_Growth, LFPR, Dependency_Ratio) %>%
  summary()

# Visualizations

# Boxplot: Dependency Ratio
ggplot(actual_data, aes(y = Dependency_Ratio)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Distribution of Age Dependency Ratio (1990–2023)",
       y = "Dependency Ratio (%)") +
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("dependency_ratio.png", width = 12, height = 8, dpi = 300)

# Scatterplot + Regression: Fertility vs GDP
ggplot(combined_data, aes(x = Fertility_Rate_Lag20, y = GDP_Growth, color = Data_Type)) +
  geom_point(size = 2.5) +
  geom_smooth(data = actual_data, method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "steelblue", "Predicted" = "red")) +
  labs(title = "GDP Growth vs Lagged Fertility Rate",
       x = "Fertility Rate (20-Year Lag)",
       y = "GDP Growth (%)",
       color = "Data Type") +
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
ggsave("regression: fertility vs GDP.png", width = 12, height = 8, dpi = 300)

# Create merged data with fertility rate and GDP growth over time
plot_data <- combined_data %>%
  select(Year, GDP_Growth, Fertility_Rate_Lag20, Data_Type)

# Create a plot with dual y-axis
# Add error bars to the existing scatterplot
ggplot(combined_data, aes(x = Fertility_Rate_Lag20, y = GDP_Growth, color = Data_Type)) +
  geom_point(size = 2.5) +
  geom_errorbar(
    data = filter(combined_data, Data_Type == "Predicted"),
    aes(ymin = Lower_CI, ymax = Upper_CI),
    width = 0.05,
    color = "firebrick"
  ) +
  geom_smooth(data = actual_data, method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "steelblue", "Predicted" = "red")) +
  labs(title = "GDP Growth vs Lagged Fertility Rate with Confidence Intervals",
       x = "Fertility Rate (20-Year Lag)",
       y = "GDP Growth (%)",
       color = "Data Type") +
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("GDP growth and fertility growth over time.png", width = 12, height = 8, dpi = 300)

# Regression line plot
ggplot(full_data, aes(x = Fertility_Rate_Lag20, y = GDP_Growth)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Regression of GDP Growth on Fertility Rate Lag 20",
       x = "Fertility Rate (Lag 20)", y = "GDP Growth (%)") + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
ggsave("Regression of GDP Growth on Fertility Rate Lag 20.png", width = 12, height = 8, dpi = 300)

# Diagnostics
par(mfrow = c(2, 2))
p <- plot(lm(GDP_Growth ~ Fertility_Rate_Lag20, data = full_data))


# --------------------------------------------------------------------------
# Checking the second hypothesis: Whether there is an association between the 
# fertility rate and overall labor force change.

# Prepare and merge data
chi_data <- fertility_armenia %>%
  left_join(labor_force_armenia, by = "Year") %>%
  drop_na(Fertility_Rate, Labor_Force)

# Categorize variables into tertiles
chi_data <- chi_data %>%
  mutate(
    Fertility_Category = cut(Fertility_Rate,
                             breaks = quantile(Fertility_Rate, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                             include.lowest = TRUE,
                             labels = c("Low", "Medium", "High")),
    Labor_Force_Category = cut(Labor_Force,
                               breaks = quantile(Labor_Force, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                               include.lowest = TRUE,
                               labels = c("Low", "Medium", "High"))
  )

# Create contingency table
contingency_table <- table(chi_data$Fertility_Category, chi_data$Labor_Force_Category)

# Run chi-square test
chi_result <- chisq.test(contingency_table)

# View test result
chi_result

# Optional: View expected counts
chi_result$expected

boxplot(GDP_Growth ~ Fertility_Category, data = full_data,
        main = "GDP Growth by Fertility Rate Category",
        xlab = "Fertility Rate Category",
        ylab = "GDP Growth (%)",
        col = c("lightblue", "lightgreen", "salmon"))

