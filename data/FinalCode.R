

#Install packages

install.packages("tidyverse")
library(tidyverse)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
install.packages("maps")
library(maps)


##INIT##


#Setting The Working Directory
setwd("~/Documents/GitHub/Obesity/data")


##Load data

education2019 <- read_csv("education2019.csv")
education2021 <- read_csv("educayion2021.csv")
education2022 <- read_csv("education2022.csv")
income2019 <- read_csv("income2019.csv")
income2021 <- read_csv("income2021.csv")
income2022 <- read_csv("income2022.csv")




###Data cleaning####

##Education data

#Selection of columns with total estimate for each year

col_numbers<- c(2,seq(3,604, by=12))
education2019_clean<-education2019[,col_numbers]

col_numbers<- c(2,seq(3,604, by=12))
education2021_clean<-education2021[,col_numbers]

col_numbers<- c(2,seq(3,604, by=12))
education2022_clean<-education2022[,col_numbers]

# Add column year

education2019_clean <- education2019_clean %>%
  mutate(year = 2019)

education2021_clean <- education2021_clean %>%
  mutate(year = 2021)

education2022_clean <- education2022_clean %>%
  mutate(year = 2022)


#Selection of Population 18-24 years with bachelor's degree or higher for each year

Bachelor_2019 <- education2019_clean[6, ]

Bachelor_2021 <- education2021_clean[6, ]

Bachelor_2022 <- education2022_clean[6, ]


#Merge education data

merged_education <- bind_rows(Bachelor_2019, Bachelor_2021, Bachelor_2022)

#Rename column

merged_education_clean <- merged_education %>%
  rename(Variable = `Label..Grouping.`)

#Match the structure of the education data for consistent analysis
#Change states names

merged_education_clean <- merged_education_clean %>%
  select(-Variable, -1) %>%
  pivot_longer(-year, names_to = "GeoName", values_to = "education")%>%
  mutate(GeoName = gsub("\\.\\.Total\\.\\.Estimate", "", GeoName))
  
# Remove dots in states names
  merged_education_clean <- merged_education_clean %>%
  mutate(GeoName = gsub("\\.", " ", GeoName))
  
#Check state names

unique(merged_education_clean$GeoName)

#Convert column education to numeric
merged_education_clean <- merged_education_clean %>%
  mutate(education = as.numeric(gsub(",", "", education)))



##Income data

#Add column year and rename column

income2019_clean <- income2019 %>%
  mutate(year = 2019)%>%
  rename(income = X2019)

income2021_clean <- income2021 %>%
  mutate(year = 2021)%>%
  rename(income = X2021)

income2022_clean <- income2022 %>%
  mutate(year = 2022)%>%
  rename(income = X2022)


#Merge income data

merged_income <- bind_rows(income2019_clean, income2021_clean, income2022_clean)

#Remove rows and columns not relevant to the analysis

merged_income_clean <- merged_income %>%
  filter(GeoName != "United States")%>%
  select(year, income, GeoName)%>%
  filter(!is.na(income))



###Creation of new variables###

#Calculation of average income per state

average_income <- merged_income_clean %>%
  group_by(GeoName) %>%
  summarise(avg_income = mean(income, na.rm = TRUE))%>%
  arrange(desc(avg_income))

#Plot figure

ggplot(average_income, aes(x = reorder(GeoName, -avg_income), y = avg_income/1e3)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Average Income by State",
    x = "State",
    y = "Average Income (in Thousands)"
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
#Calculation of average education per state and remove commas and convert to numeric

average_education <- merged_education_clean %>%
  mutate(education = as.numeric(gsub(",", "", education))) %>%
  group_by(GeoName) %>%
  summarise(avg_education = mean(education, na.rm = TRUE))%>%
  arrange(desc(avg_education))

#Plot figure

ggplot(average_education, aes(x = reorder(GeoName, -avg_education), y = avg_education/1e3)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Average Number of People with Higher Education by State",
    x = "State",
    y = "Average People (in Thousands)"
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

###Visualization###

##Visualize spatial variation

#Install packages

library(ggplot2)
library(dplyr)
library(maps)

#Education data
# Map data for U.S. states

us_states <- map_data("state")

#Change education data to match map data

education_map <- average_education %>%
  mutate(region = tolower(GeoName))

#Merge the education data to the US map data

education_map <- us_states %>%
  left_join(education_map, by = "region")

# Plot average education per state with color scale in millions

ggplot(education_map, aes(x = long, y = lat, group = group, fill = avg_education)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  labs(
    title = "Average Number of People with Higher Education by State",
    fill = "Average Education (Millions)"
  ) +
  scale_fill_viridis_c(option = "plasma", direction = -1, labels = scales::label_number(suffix = "M", accuracy = 1)) +
  theme_void()


#Income data


# Map data for U.S. states

us_states <- map_data("state")

#Change education data to match map data

income_map <- average_income %>%
  mutate(region = tolower(GeoName))

#Merge the education data to the US map data

income_map <- us_states %>%
  left_join(income_map, by = "region")

# Plot average income per state with color scale in millions

ggplot(income_map, aes(x = long, y = lat, group = group, fill = avg_income)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  labs(
    title = "Average Annual Income by State",
    fill = "Average Income (Millions)"
  ) +
  scale_fill_viridis_c(option = "plasma", direction = -1, labels = scales::label_number(suffix = "M", accuracy = 1)) +
  theme_void()


##Visualize Sub-population variation

# Create a data frame that maps states to regions
state_regions <- data.frame(
  GeoName = c(
    "Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont",
    "New Jersey", "New York", "Pennsylvania",
    "Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin",
    "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota",
    "Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", 
    "Virginia", "Washington D.C.", "West Virginia", "Alabama", "Kentucky", "Mississippi", 
    "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas",
    "Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", 
    "Utah", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington"
  ),
  region = c(
    rep("Northeast", 9),
    rep("Midwest", 12),
    rep("South", 17),
    rep("West", 13)
  )
)

#Merge average education and regions
education_states <- average_education %>%
  inner_join(state_regions, by = "GeoName")

#Plot
ggplot(education_states, aes(x = region, y = avg_education / 1e3, fill = region)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Education Distribution by Region",
    x = "Region",
    y = "People (in Thousands)"
  )
##Visualize Temporal Variation

# Merge education data and income data

merged_data <-merged_education_clean%>%
  left_join(merged_income_clean, by = c("GeoName", "year"))

#Select West Virginia because it is the state with the highest obesity rate

west_virginia_data <- merged_data %>%
  filter(GeoName == "West Virginia") %>%
  select(year, income, education)

# Prepare date before plotting
plot_temporal <- west_virginia_data %>%
  pivot_longer(cols = c(income, education),
               names_to = "Metric", values_to = "value")

# Plot
ggplot(plot_temporal, aes(x = year, y = value, color = Metric)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  labs(
    title = "Income and Education Over Time in West Virginia",
    x = "Year",
    y = "Value",
    color = "Metric"
  ) +
  theme_minimal()
