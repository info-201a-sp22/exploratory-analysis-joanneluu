library(ggplot2)
library(dplyr)

dog_data <- read.csv("https://raw.githubusercontent.com/dennywu84/Final-Project/main/dog-data.csv", stringsAsFactors = F )

top_10_dog_names <- dog_data %>% 
  filter(AnimalName != "NAME NOT PROVIDED" & AnimalName != "UNKNOWN") %>% 
  group_by(AnimalName) %>% 
  summarize(n = n()) %>% 
  top_n(10)

bella <- dog_data %>% 
  filter(AnimalName == "BELLA") %>% 
  group_by(AgeAsOf2015) %>% 
  summarize(n = n()) %>% 
  mutate(Name = paste("Bella"))

max <- dog_data %>% 
  filter(AnimalName == "MAX") %>% 
  group_by(AgeAsOf2015) %>% 
  summarize(n = n()) %>% 
  mutate(Name = paste("Max"))

charlie <- dog_data %>% 
  filter(AnimalName == "CHARLIE") %>% 
  group_by(AgeAsOf2015) %>% 
  summarize(n = n()) %>% 
  mutate(Name = paste("Charlie"))

coco <- dog_data %>% 
  filter(AnimalName == "COCO") %>% 
  group_by(AgeAsOf2015) %>% 
  summarize(n = n()) %>% 
  mutate(Name = paste("Coco"))

rocky <- dog_data %>% 
  filter(AnimalName == "ROCKY") %>% 
  group_by(AgeAsOf2015) %>% 
  summarize(n = n()) %>% 
  mutate(Name = paste("Rocky"))

lola <- dog_data %>% 
  filter(AnimalName == "LOLA") %>% 
  group_by(AgeAsOf2015) %>% 
  summarize(n = n()) %>% 
  mutate(Name = paste("Lola"))

lucky <- dog_data %>% 
  filter(AnimalName == "LUCKY") %>% 
  group_by(AgeAsOf2015) %>% 
  summarize(n = n()) %>% 
  mutate(Name = paste("Lucky"))

lucy <- dog_data %>% 
  filter(AnimalName == "LUCY") %>% 
  group_by(AgeAsOf2015) %>% 
  summarize(n = n()) %>% 
  mutate(Name = paste("Lucy"))

buddy <- dog_data %>% 
  filter(AnimalName == "BUDDY") %>% 
  group_by(AgeAsOf2015) %>% 
  summarize(n = n()) %>% 
  mutate(Name = paste("Buddy"))

daisy <- dog_data %>% 
  filter(AnimalName == "DAISY") %>% 
  group_by(AgeAsOf2015) %>% 
  summarize(n = n()) %>% 
  mutate(Name = paste("Daisy"))

total <- rbind(bella, max, charlie, coco, rocky, lola, lucky, lucy, buddy, daisy)

total$AgeAsOf2015 <- as.numeric(as.character(total$AgeAsOf2015))

ggplot(data = total) +
  geom_line(aes(x = AgeAsOf2015, y = n, color = Name)) +
  facet_wrap(~Name) + 
  labs( title = "The Relationship Between Dogs Names and Their Ages",
      x = "Ages",
      y = "Dogs Names")
