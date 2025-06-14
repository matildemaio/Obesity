install.packages("tidyverse")
library(tidyverse)


library(dplyr)
library(purrr)



education2021 = educayion2021
df_list <- list(education2019, education2021, education2022)

combined<-imap_dfr(df_list, ~mutate(.x, year=.y))

combined$year <- combined$year %>% replace(combined$year == 1, 2019)
combined$year <- combined$year %>% replace(combined$year == 2, 2021)
combined$year <- combined$year %>% replace(combined$year == 3, 2022)

col_numbers <- c(2, seq(3,604, by = 12))
combined<-combined[,col_numbers]

library(dplyr)
library(tidyr)
income2019 <- income2019 %>% rename(income = X2019)
income2021 <- income2021 %>% rename(income = X2021)
income2022 <- income2022 %>% rename(income = X2022)
income2019 <- income2019 %>% mutate(year = 2019)
income2021 <- income2021 %>% mutate(year = 2021)
income2022 <- income2022 %>% mutate(year = 2022)
combined_income <- bind_rows(income2019, income2021, income2022)
average_income <- combined_income %>%
  group_by(GeoName) %>%
  summarise(avg_income = mean(income, na.rm = TRUE)) %>%
  arrange(desc(avg_income))  # optional: sort by highest income