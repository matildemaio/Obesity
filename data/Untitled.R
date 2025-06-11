library(dplyr)
library(purrr)
education2021 = educayion2021
df_list <- list(education2019, education2021, education2022)
combined<-imap_dfr(df_list, ~mutate(.x, year=.y))
combined <- combined %>%
  mutate(year = case_when(
    year == 1 ~ 2019,
    year == 2 ~ 2021,
    year == 3 ~ 2022,
    TRUE ~ year
    