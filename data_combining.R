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