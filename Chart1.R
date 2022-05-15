library(ggplot2)
library(dplyr)
dog_data <- read.csv("https://raw.githubusercontent.com/dennywu84/Final-Project/main/dog-data.csv", stringsAsFactors = F )

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

total <- nrow(dog_data)
Name <- c("top_5", "top_5to10", "top_10to15", "top_15to20", "other")
n <- c(top_5, top_5to10, top_10to15, top_15to20, other)
pct <- n / total
pct <- paste(as.character(round(n/total * 100, digits = 2)), "%")

data <- data.frame(Name, n, pct)

ggplot(data, aes(x = "", y = n, fill = Name)) +
  geom_col() +
  coord_polar("y") +
  geom_text(aes(label = pct, x = 1.0), position = position_stack(vjust = 0.5), size = 4) +
  theme_void() + 
  labs(title = "Comparing the Percentage of the Number of Top 20 Name") +
  scale_fill_discrete(name = "Group", 
                      labels = c("Other", "Top 10 to 15 Name", "Top 15 to 20 Name", 
                                 "Top 5 Name", "Top 5 to 10 Name"))






