#Loading packages 
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



income2019 <- income2019 %>% mutate(year = 2019)
income2021 <- income2021 %>% mutate(year = 2021)
income2022 <- income2022 %>% mutate(year = 2022)

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

ggplot(average_income_states, aes(x = reorder(GeoName, -avg_income), y = avg_income / 1e6)) +
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






