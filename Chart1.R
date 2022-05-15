library(ggplot2)
dog_data <- read.csv("https://raw.githubusercontent.com/dennywu84/Final-Project/main/dog-data.csv", stringsAsFactors = F )

dog_data <- dog_data %>% 
  filter(BreedName != "Unknown")

breed_n <- dog_data %>%
  group_by(BreedName) %>%
  summarize(n = n()) %>% 
  filter(BreedName != "Unknown") %>% 
  top_n(20)

top_5 <- 3853 + 4200 + 5528 + 6659 + 6978
top_5to10 <- 3195 + 2763 + 2525 + 2093 + 1970
top_10to15 <- 1897 + 1774 + 1759 + 1751 + 1718
top_15to20 <- 1480 + 1315 + 1289 + 1267 + 1242
other <- nrow(dog_data) - sum(breed_n$n)

Name <- c("top_5", "top_5to10", "top_10to15", "top_15to20", "other")
n <- c(top_5, top_5to10, top_10to15, top_15to20, other)

data <- data.frame(Name, n)

ggplot(data, aes(x = "", y = n, fill = Name)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  labs(title = "Comparing the percentage of the number of top 20 name")




