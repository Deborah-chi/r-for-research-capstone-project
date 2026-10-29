
# Load packages ------------------------------------------------------------

library(tidyverse)

library(janitor)

library(readr)

library(skimr)

library(dplyr)

# Import data -------------------------------------------------------------

cuckoo_data <- read_csv("https://raw.githubusercontent.com/xrander/bootcamp-test/master/data/cuckoo.csv") |> 
  janitor::clean_names()

# Exploratory Data Analysis -----------------------------------------------

#Look into the dataset to understand how it is and to know if it's messy
head(cuckoo_data)

tail(cuckoo_data)

set.seed(90)
car::some(cuckoo_data)

str(cuckoo_data)

# Confirms number of species
cuckoo_data |> 
  count(species)

## Summarize and group_by for proper statistics ---------------------------

summary(cuckoo_data)

# Overall Summary Statistics
cuckoo_data |>
  summarise(
    min_length = min(length),
    max_length = max(length),
    mean_length = mean(length),
    median_length = median(length),
    
    min_breadth = min(breadth),
    max_breadth = max(breadth),
    mean_breadth = mean(breadth),
    median_breadth = median(breadth)
  )

# Group_by summary of egg sizes BY SPECIES
cuckoo_data_summary <- cuckoo_data |>
  group_by(species) |>
  summarise(
    count = n(),
    avg_length = mean(length),
    avg_breadth = mean(breadth)
  )

cuckoo_data_summary

# Visualization using box plot --------------------------------------------

# Boxplot for Breadth
plot_breath <- ggplot(cuckoo_data,
       mapping = aes(x = species, y = breadth, fill = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Cuckoo Eggs Breadth by Host Species",
    x = "Host Species",
    y = "Breadth (mm)"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_breath

# Boxplot for Length
plot_length <- ggplot(cuckoo_data,
       mapping = aes(x = species, y = length, fill = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Cuckoo Eggs Breadth by Host Species",
    x = "Host Species",
    y = "Length (mm)"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_length

# Inferential statistic analysis ------------------------------------------

# One-way ANOVA for Egg BREADTH by Species
breadth_model <- aov(breadth ~ species, data = cuckoo_data)
summary(breadth_model)

# One-way ANOVA for Egg LENGTH by Species
length_model <- aov(length ~ species, data = cuckoo_data)
summary(length_model)
