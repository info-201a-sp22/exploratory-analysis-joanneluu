library("dplyr")

dog_data <- read.csv("https://raw.githubusercontent.com/dennywu84/Final-Project/main/dog-data.csv", stringsAsFactors = F )


most_popular_name <- dog_data %>% group_by(AnimalName) %>% 
  summarise(n = n()) 
most_popular_name <- most_popular_name[most_popular_name$AnimalName != "UNKNOWN", ]
most_popular_name <- most_popular_name[most_popular_name$AnimalName != "NAME NOT PROVIDED", ]
most_popular_name <- most_popular_name %>% filter(n == max(n))