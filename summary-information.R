library("dplyr")

dog_data <- read.csv("https://raw.githubusercontent.com/dennywu84/Final-Project/main/dog-data.csv", stringsAsFactors = F )

summary_information_func <- function(dog_data) {
summary_info <- list()

most_popular_name <- dog_data %>% group_by(AnimalName) %>% 
  summarise(n = n()) 
most_popular_name <- most_popular_name[most_popular_name$AnimalName != "UNKNOWN", ]
most_popular_name <- most_popular_name[most_popular_name$AnimalName != "NAME NOT PROVIDED", ]
summary_info$most_popular_name <- most_popular_name %>% filter(n == max(n, na.rm = TRUE)) %>% pull(AnimalName)

most_popular_breed <- dog_data %>% group_by(BreedName) %>% 
  summarise(n = n())
most_popular_breed <- most_popular_breed[most_popular_breed$BreedName != "Unknown", ]
summary_info$most_popular_breed <- most_popular_breed %>% filter(n == max(n, na.rm = TRUE)) %>% 
  pull(BreedName)

proportion_top_20 <- dog_data %>% group_by(BreedName) %>% 
  summarise(n = n())
proportion_top_20 <- proportion_top_20[proportion_top_20$BreedName != "Unknown", ]
total_dogs <- proportion_top_20 %>% summarise(sum(n))
proportion_top_20 <- proportion_top_20 %>% top_n(20) %>% 
  summarise(sum(n))
proportion_top_20 <- proportion_top_20 / total_dogs
summary_info$proportion_top_20 <- proportion_top_20 %>% pull("sum(n)")

summary_info$borough_largest_population <- dog_data %>% group_by(Borough) %>% 
  summarise(population = n()) %>% 
  filter(population == max(population)) %>% 
  pull(Borough)

summary_info$borough_most_unique_names <- dog_data %>% group_by(Borough) %>% 
  distinct(AnimalName) %>% 
  summarise(number_of_names = n()) %>% 
  filter(number_of_names == max(number_of_names)) %>% 
  pull(Borough)
}

summary_information_func(dog_data)

  
