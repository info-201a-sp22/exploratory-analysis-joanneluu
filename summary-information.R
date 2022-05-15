library("dplyr")
library("scales")

dog_data <- read.csv("https://raw.githubusercontent.com/dennywu84/Final-Project/main/dog-data.csv", stringsAsFactors = F )

summary_info <- list()

most_popular_name <- dog_data %>% group_by(AnimalName) %>% 
  summarise(n = n()) 
most_popular_name <- most_popular_name[most_popular_name$AnimalName != "UNKNOWN", ]
most_popular_name <- most_popular_name[most_popular_name$AnimalName != "NAME NOT PROVIDED", ]
most_popular_name <- most_popular_name %>% filter(n == max(n, na.rm = TRUE)) %>% pull(AnimalName)
most_popular_name <- "Bella"
summary_info$most_popular_name <- most_popular_name

most_popular_breed <- dog_data %>% group_by(BreedName) %>% 
  summarise(n = n())
most_popular_breed <- most_popular_breed[most_popular_breed$BreedName != "Unknown", ]
most_popular_breed <- most_popular_breed %>% filter(n == max(n, na.rm = TRUE)) %>%
  pull(BreedName)
summary_info$most_popular_breed <- most_popular_breed

proportion_top_20 <- dog_data %>% group_by(BreedName) %>% 
  summarise(n = n())
total_dogs <- proportion_top_20 %>% summarise(sum(n))
proportion_top_20 <- proportion_top_20[proportion_top_20$BreedName != "Unknown", ]
proportion_top_20 <- proportion_top_20 %>% top_n(20) %>% 
  summarise(sum(n))
proportion_top_20 <- proportion_top_20 / total_dogs
proportion_top_20 <- percent(proportion_top_20 %>% pull("sum(n)"), .01)
summary_info$proportion_top_20 <- proportion_top_20

borough_largest_population <- dog_data %>% group_by(Borough) %>% 
  summarise(population = n()) %>% 
  filter(population == max(population)) %>% 
  pull(Borough)
summary_info$borough_largest_population <- borough_largest_population

borough_most_unique_names <- dog_data %>% group_by(Borough) %>% 
  distinct(AnimalName) %>% 
  summarise(number_of_names = n()) %>% 
  filter(number_of_names == max(number_of_names)) %>% 
  pull(Borough)
summary_info$borough_most_unique_names <- borough_most_unique_names