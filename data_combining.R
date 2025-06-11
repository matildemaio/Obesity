install.packages("tidyverse")
library(tidyverse)


library(dplyr)
library(purrr)



education2021 = educayion2021
df_list <- list(education2019, education2021, education2022)

combined<-imap_dfr(df_list, ~mutate(.x, year=.y))
