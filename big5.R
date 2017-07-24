

big5 = read.csv(file = "big5.csv")


library(ggplot2)
hist(big5$big5_item_c)

ggplot(data = big5, aes(big5$big5_item_c)) + geom_histogram()

Conscientiousness = big5$big5_item_c
Openness = big5$big5_item_o
Extroversion = big5$big5_item_e
Agreeableness = big5$big5_item_a
Neuroticism = big5$big5_item_n

ggplot(data = big5, aes(Neuroticism)) + 
  geom_histogram(fill="blue")
