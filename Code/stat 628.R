library(tidyverse)

data1 <- read.csv("/home/basil3325/Downloads/yelp_reviews_RV_categories3cheap.csv")

data2 <- read.csv("/home/basil3325/Downloads/yelp_reviews_RV_categories3expensive.csv")

data1a <- data1[!data1$brunch == "", ]

data2a <- data2[!data2$brunch == "", ]

unique_values1 <- sort(unique(data1a$brunch))

unique_values1

unique_values2 <- sort(unique(data2a$brunch))

unique_values2

data1a <- data1a %>% 
  mutate(grouped = case_when(
    brunch %in% c("False","None" ) ~ "False",
    brunch %in% c("True") ~ "True",
    # Add more cases as needed
    TRUE ~ as.character(brunch)
  ))

data2a <- data2a %>% 
  mutate(grouped = case_when(
    brunch %in% c("False") ~ "False",
    brunch %in% c("True") ~ "True",
    # Add more cases as needed
    TRUE ~ as.character(brunch)
  ))

t_test_result_1 <- wilcox.test(review_stars ~ grouped, data = data1a)

t_test_result_1

t_test_result_2<-  wilcox.test(review_stars ~ grouped, data = data2a)

t_test_result_2

data1a %>%
  group_by(grouped) %>%
  summarise(mean_mpg = mean(review_stars)) %>%
  ggplot(aes(x = grouped, y = mean_mpg, fill = factor(grouped))) +
  geom_bar(stat = "identity") +
  labs(x = "Brunch",
       y = "Average rating") +
  theme_minimal()