library("dplyr")
library("scales")

dog_data <- read.csv("https://raw.githubusercontent.com/dennywu84/Final-Project/main/dog-data.csv", stringsAsFactors = F )

table_data <- list()

proportion_top_20 <- dog_data %>% group_by(BreedName) %>% 
  summarise(n = n())

total_dogs <- proportion_top_20 %>% summarise(sum(n)) %>% pull("sum(n)")
table_data$total_dogs <- total_dogs

proportion_top_20 <- proportion_top_20[proportion_top_20$BreedName != "Unknown", ]
proportion_top_20 <- proportion_top_20 %>% top_n(20) %>% 
  summarise(sum(n)) %>%  pull("sum(n)")
table_data$proportion_top_20 <- proportion_top_20

percent_in_top_20 <- proportion_top_20 / total_dogs
table_data$percent_in_top_20 <- percent(percent_in_top_20, .01)

number_unique_names <- dog_data %>% distinct(AnimalName)
table_data$number_unique_names <- nrow(number_unique_names)

number_of_unknown_names <- dog_data %>% group_by(AnimalName) %>% 
  summarise(n = n()) 
unknown <- number_of_unknown_names[number_of_unknown_names$AnimalName == "UNKNOWN", ]
unknown <- unknown %>% filter(n == max(n, na.rm = TRUE)) %>% 
  pull(n)
not_given <- number_of_unknown_names[number_of_unknown_names$AnimalName == "NAME NOT PROVIDED", ]
not_given <- not_given %>% filter(n == max(n, na.rm = TRUE)) %>% 
  pull(n)
table_data$number_of_unknown_names <- unknown + not_given

table <- table(data.frame(table_data))
table