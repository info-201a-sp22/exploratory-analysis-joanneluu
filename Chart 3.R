library(dplyr)
library(ggplot2)

dog_data <- read.csv("https://raw.githubusercontent.com/dennywu84/Final-Project/main/dog-data.csv", stringsAsFactors = F )

dog_data <- dog_data %>% 
  filter(BreedName != "Unknown")

borough <- dog_data %>% 
  filter(Borough != "Null") %>% 
  group_by(Brorough = tolower(Borough))  %>% 
  summarize(n = n())

bronx <- dog_data %>% 
  filter(Borough == "Bronx" | Borough == "BRONX") %>% 
  group_by(BreedName) %>% 
  summarize(n = n()) %>% 
  top_n(5) %>% 
  mutate(Borough = paste("Bronx"))

brooklyn <- dog_data %>% 
  filter(Borough == "Brooklyn" | Borough == "BROOKLYN") %>% 
  group_by(BreedName) %>% 
  summarize(n = n()) %>% 
  top_n(5) %>% 
  mutate(Borough = paste("BRooklyn"))


manhattan <- dog_data %>% 
  filter(Borough == "Manhattan" | Borough == "MANHATTAN") %>% 
  group_by(BreedName) %>% 
  summarize(n = n()) %>% 
  top_n(5) %>% 
  mutate(Borough = paste("Manhattan"))


queens <- dog_data %>% 
  filter(Borough == "Queens" | Borough == "QUEENS") %>% 
  group_by(BreedName) %>% 
  summarize(n = n()) %>% 
  top_n(5) %>% 
  mutate(Borough = paste("Queens"))

staten_island <- dog_data %>% 
  filter(Borough == "Staten Island" | Borough == "STATEN ISLAND") %>% 
  group_by(BreedName) %>% 
  summarize(n = n()) %>% 
  top_n(5) %>% 
  mutate(Borough = paste("Staten Island"))

ggplot(data = bronx) +
  geom_col(aes(x = BreedName, y = n, color = BreedName, fill = BreedName)) +
  facet_wrap(~Borough) +
  labs( title = "Top 5 Dog Breeds in Bronx",
        x = "Dog Breeds",
        y = "Dog Number")

ggplot(data = brooklyn) +
  geom_col(aes(x = BreedName, y = n, color = BreedName, fill = BreedName)) +
  facet_wrap(~Borough) +
  labs( title = "Top 5 Dog Breeds in Brooklyn",
        x = "Dog Breeds",
        y = "Dog Number")

ggplot(data = manhattan) +
  geom_col(aes(x = BreedName, y = n, color = BreedName, fill = BreedName)) +
  facet_wrap(~Borough) +
  labs( title = "Top 5 Dog Breeds in Manhattan",
        x = "Dog Breeds",
        y = "Dog Number")

ggplot(data = queens) +
  geom_col(aes(x = BreedName, y = n, color = BreedName, fill = BreedName)) +
  facet_wrap(~Borough) +
  labs( title = "Top 5 Dog Breeds in Queens",
        x = "Dog Breeds",
        y = "Dog Number")

ggplot(data = staten_island) +
  geom_col(aes(x = BreedName, y = n, color = BreedName, fill = BreedName)) +
  facet_wrap(~Borough) +
  labs( title = "Top 5 Dog Breeds in Staten Island",
        x = "Dog Breeds",
        y = "Dog Number")



