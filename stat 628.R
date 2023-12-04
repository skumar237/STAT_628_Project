library(tidyverse)

data1 <- read.csv("/home/basil3325/Downloads/yelp_reviews_RV_categories3cheap.csv")

data2 <- read.csv("/home/basil3325/Downloads/yelp_reviews_RV_categories3expensive.csv")

data1a <- data1[!data1$trendy == "", ]

data2a <- data2[!data2$trendy == "", ]

unique_values1 <- sort(unique(data1a$trendy))

unique_values1

unique_values2 <- sort(unique(data2a$trendy))

unique_values2

data1a <- data1a %>% 
  mutate(grouped = case_when(
    trendy %in% c("False","None" ) ~ "False",
    trendy %in% c("True") ~ "True",
    # Add more cases as needed
    TRUE ~ as.character(trendy)
  ))

data2a <- data2a %>% 
  mutate(grouped = case_when(
    trendy %in% c("False") ~ "False",
    trendy %in% c("True") ~ "True",
    # Add more cases as needed
    TRUE ~ as.character(trendy)
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
  labs(x = "Trendy",
       y = "Average rating") +
  theme_minimal()