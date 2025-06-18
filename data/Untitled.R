install.packages("tidyverse")
library(tidyverse)

library(dplyr)
library(purrr)


#Correction of name
education2021 = educayion2021

#Create list of education data
df_list <- list(education2019, education2021, education2022)

#Merge education data  
combined<-imap_dfr(df_list, ~mutate(.x, year=.y))

#Replace year codes with actual years 
combined$year <- combined$year %>% replace(combined$year == 1, 2019)
combined$year <- combined$year %>% replace(combined$year == 2, 2021)
combined$year <- combined$year %>% replace(combined$year == 3, 2022)


#Selection of columns
col_numbers <- c(2, seq(3,604, by = 12))
combined<-combined[,col_numbers]

library(dplyr)
library(tidyr)



# Rename the income columns and add year
income2019 <- income2019 %>%
  mutate(year = 2019)

income2021 <- income2021 %>%
  mutate(year = 2021)

income2022 <- income2022 %>%
  mutate(year = 2022)
combined_income <- bind_rows(income2019, income2021, income2022)

average_income <- combined_income %>%
  filter(!GeoName %in% c("United States", "2025.")) %>%
  group_by(GeoName) %>%
  summarise(avg_income = mean(income, na.rm = TRUE)) %>%
  arrange(desc(avg_income))


# Now bind rows
combined_income <- bind_rows(income2019, income2021, income2022)


#Merge income data
combined_income <- bind_rows(income2019, income2021, income2022)

#Calculation of average income per state
average_income <- combined_income %>%
  group_by(GeoName) %>%
  summarise(avg_income = mean(income, na.rm = TRUE)) %>%
  
  #Sort form highest to lowest
  arrange(desc(avg_income))

library(tidyverse)

# Step 1: Extract for each year
edu2019 <- education2019 %>%
  filter(str_detect(`Label..Grouping.`, "Bachelor's degree or higher")) %>%
  mutate(year = 2019)

edu2021 <- education2021 %>%
  filter(str_detect(`Label..Grouping.`, "Bachelor's degree or higher")) %>%
  mutate(year = 2021)

edu2022 <- education2022 %>%
  filter(str_detect(`Label..Grouping.`, "Bachelor's degree or higher")) %>%
  mutate(year = 2022)

# Step 2: Combine
education_combined <- bind_rows(edu2019, edu2021, edu2022)

# Step 3: Pivot longer
education_long <- education_combined %>%
  select(-`Label..Grouping.`, -`...1`) %>%
  pivot_longer(-year, names_to = "GeoName", values_to = "Count")

# Step 4: Clean & average
education_long$Count <- as.numeric(gsub(",", "", education_long$Count))

average_education <- education_long %>%
  filter(!GeoName %in% c("United States", "2025."), !is.na(Count)) %>%
  group_by(GeoName) %>%
  summarise(avg_education = mean(Count, na.rm = TRUE)) %>%
  arrange(desc(avg_education))
average_education_clean <- average_education %>%
  filter(str_detect(GeoName, "\\.\\.Total\\.\\.Estimate")) %>%
  mutate(
    GeoName = str_replace(GeoName, "\\.\\.Total\\.\\.Estimate", "")
  )
View(average_education_clean)

ggplot(average_income, aes(x = reorder(GeoName, -avg_income), y = avg_income / 1e6)) +
  geom_point() +
  labs(
    title = "Average Income by State (in Millions)",
    x = "State",
    y = "Average Income (Millions USD)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



library(ggplot2)

ggplot(average_education_clean, aes(x = reorder(GeoName, -avg_education), y = avg_education)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Number with Bachelor's Degree or Higher by State",
    x = "State",
    y = "Average Education Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(ggplot2)
library(dplyr)
library(maps)
# Get map data for U.S. states
us_states <- map_data("state")

# Prepare education data: lowercase state names to match map data
education_map <- average_education_clean %>%
  mutate(region = tolower(GeoName))
map_edu <- us_states %>%
  left_join(education_map, by = "region")
ggplot(map_edu, aes(x = long, y = lat, group = group, fill = avg_education)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  labs(
    title = "Average Bachelor's Degree or Higher by State",
    fill = "Avg. Education"
  ) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  theme_void()

unique(average_education_clean$GeoName)




average_income_states <- average_income %>%
  filter(GeoName != "United States") %>%
  mutate(region = tolower(GeoName))
library(ggplot2)
library(maps)

us_states <- map_data("state")
map_income <- us_states %>%
  left_join(average_income_states, by = "region")
ggplot(map_income, aes(x = long, y = lat, group = group, fill = avg_income)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  labs(
    title = "Average Income by State",
    fill = "Avg. Income"
  ) +
  scale_fill_viridis_c(option = "cividis", direction = -1) +
  theme_void()

# Filter for California and Vermont
income_subset <- average_income %>%
  filter(GeoName %in% c("California", "Vermont")) %>%
  mutate(Type = "Income", Value = avg_income / 1e6) %>%
  select(GeoName, Type, Value)

education_subset <- average_education_clean %>%
  filter(GeoName %in% c("California", "Vermont")) %>%
  mutate(Type = "Education", Value = avg_education / 1e6) %>%
  select(GeoName, Type, Value)

# Combine both
combined_plot_data <- bind_rows(income_subset, education_subset)

ggplot(combined_plot_data, aes(x = GeoName, y = Value, fill = Type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Income and Education for California and Vermont",
    x = "State",
    y = "Millions (USD / People)",
    fill = "Metric"
  ) +
  theme_minimal()

# Step 1: Filter and summarize income
income_ca <- combined_income %>%
  filter(GeoName == "California") %>%
  group_by(year) %>%
  summarise(Value = mean(income, na.rm = TRUE)) %>%
  mutate(Type = "Income")

# Step 2: Filter and summarize education (corrected)
education_ca <- education_long %>%
  filter(GeoName == "California..Total..Estimate") %>%
  group_by(year) %>%
  summarise(Value = mean(Count, na.rm = TRUE)) %>%
  mutate(Type = "Education")

# Step 3: Combine and plot
line_data <- bind_rows(income_ca, education_ca)

ggplot(line_data, aes(x = year, y = Value, color = Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  labs(
    title = "Income and Education Trend in California",
    x = "Year",
    y = "Value (USD / People)",
    color = "Metric"
  ) +
  scale_x_continuous(breaks = c(2019, 2020, 2021, 2022)) +
  theme_minimal()
