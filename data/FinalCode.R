#Correction of name
education2021 = educayion2021


#Install packages

install.packages("tidyverse")
library(tidyverse)

library(dplyr)
library(purrr)
library(tidyr)


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
  
#Calculation of average education per state and remove commas and convert to numeric

average_education <- merged_education_clean %>%
  mutate(education = as.numeric(gsub(",", "", education))) %>%
  group_by(GeoName) %>%
  summarise(avg_education = mean(education, na.rm = TRUE))%>%
  arrange(desc(avg_education))


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

#Merge average education and average income

merged_averages <- average_education %>%
  inner_join(average_income, by = "GeoName")

#Select 10 states with highest average education

top10_education <- merged_averages %>%
  arrange(desc(avg_education)) %>%
  slice_head(n = 10)%>%
  mutate(group = "Top 10 Educated")

# Select 10 states with lowest average education

bottom10_education <- merged_averages %>%
  arrange(avg_education) %>%
  slice_head(n = 10) %>%
  mutate(group = "Bottom 10 Educated")

# Merge groups and adjust scale to millions
combined_groups <- bind_rows(top10_education, bottom10_education)%>%
  mutate(
  avg_education = avg_education / 1e6,
  avg_income = avg_income / 1e6
)

#Prepare data before plotting 

plot_sub_pop <- combined_groups %>%
  select(GeoName, avg_education, avg_income, group) %>%
  pivot_longer(cols = c(avg_education, avg_income),
               names_to = "Metric", values_to = "Value")
plot_sub_pop <- plot_sub_pop %>%
  mutate(Metric = recode(Metric,
                         avg_education = "Average Education",
                         avg_income = "Average Income"))
# Plot
ggplot(plot_sub_pop, aes(x = reorder(GeoName, -Value), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ group, scales = "free_x") +
  labs(title = "Comparison of Income and Education Levels",
       x = "State", y = "Value (Millions)", fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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
