
#Correction of name
education2021 = educayion2021


install.packages("tidyverse")
library(tidyverse)

library(dplyr)
library(purrr)


library(dplyr)
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

merged_education <- merged_education %>%
  rename(Variable = `Label..Grouping.`)

#Match the structure of the income data for consistent analysis

merged_education <- merged_education %>%
  select(-Variable, -1) %>%
  pivot_longer(-year, names_to = "GeoName", values_to = "education")%>%
  mutate(GeoName = gsub("\\.\\.Total\\.\\.Estimate", "", GeoName))

##Income data

#Add column year

income2019 <- income2019 %>%
  mutate(year = 2019)

income2021 <- income2021 %>%
  mutate(year = 2021)

income2022 <- income2022 %>%
  mutate(year = 2022)

#Merge income data

merged_income <- bind_rows(income2019, income2021, income2022)

#Remove rows not relevant to the analysis

merged_income <- merged_income %>%
  filter(!is.na(as.numeric(GeoFips)))%>%
  filter(GeoName != "United States")


###Creation of new variables###

#Calculation of average income per state

average_income <- merged_income %>%
  group_by(GeoName) %>%
  summarise(avg_income = mean(income, na.rm = TRUE))%>%
  arrange(desc(avg_income))
  
#Calculation of average education per state and remove commas and convert to numeric

average_education <- merged_education %>%
  mutate(education = as.numeric(gsub(",", "", education))) %>%
  group_by(GeoName) %>%
  summarise(avg_education = mean(education, na.rm = TRUE))%>%
  arrange(desc(avg_education))



#I still need to do this part
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



#NEED TO REMOVE THIS 
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




library(tidyverse)

# Filter top 10 highest and lowest income states (excluding United States)
top10_income <- average_income %>%
  filter(GeoName != "United States") %>%
  arrange(desc(avg_income)) %>%
  slice(1:10)

bottom10_income <- average_income %>%
  filter(GeoName != "United States") %>%
  arrange(avg_income) %>%
  slice(1:10)

# Combine both sets of states
selected_states <- bind_rows(top10_income, bottom10_income)

# Join with education data
comparison_data <- selected_states %>%
  left_join(average_education_clean, by = "GeoName") %>%
  select(GeoName, avg_income, avg_education) %>%
  mutate(
    avg_income = avg_income / 1e6,
    avg_education = avg_education / 1e6
  )

# Reshape for plotting
comparison_long <- comparison_data %>%
  pivot_longer(cols = c(avg_income, avg_education), 
               names_to = "Metric", 
               values_to = "Value") %>%
  mutate(Metric = recode(Metric, 
                         avg_income = "Income", 
                         avg_education = "Education"))

# Plot
ggplot(comparison_long, aes(x = reorder(GeoName, -Value), y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  labs(
    title = "Top & Bottom 10 States: Income vs Education",
    x = "State",
    y = "Millions (USD / People)",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(tidyverse)

# Step 1: Get top and bottom 10 income states
top10_income <- average_income %>%
  filter(GeoName != "United States") %>%
  arrange(desc(avg_income)) %>%
  slice(1:10)

bottom10_income <- average_income %>%
  filter(GeoName != "United States") %>%
  arrange(avg_income) %>%
  slice(1:10)

# Step 2: Combine them
selected_states <- bind_rows(top10_income, bottom10_income)

# Step 3: Join with education data
comparison_data <- selected_states %>%
  left_join(average_education_clean, by = "GeoName") %>%
  select(GeoName, avg_income, avg_education) %>%
  mutate(
    avg_income = avg_income / 1e6,
    avg_education = avg_education / 1e6
  )

# Step 4: Convert to long format for boxplot
comparison_long <- comparison_data %>%
  pivot_longer(cols = c(avg_income, avg_education),
               names_to = "Metric",
               values_to = "Value") %>%
  mutate(Metric = recode(Metric,
                         avg_income = "Income",
                         avg_education = "Education"))

# Step 5: Plot box plot
ggplot(comparison_long, aes(x = Metric, y = Value, fill = Metric)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Income and Education\nTop & Bottom 10 States by Income",
    x = "Metric",
    y = "Millions (USD / People)")
  theme_minimal()
