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

table <- table(table_data)
#table <- prop.table(table)
#table
#names(table) <- ("Total Number of Dogs")

#data <- matrix(data = NA, nrow = 1, ncol = 4, byrow = F)
#colnames(data) <- ('Total Number of Dogs', 'Number of Unique Names', 'Number of Dogs with a Top 20 Name', 'Percent of Dogs With a Top 20 Name')