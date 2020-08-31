install.packages("tidytext")

library(tidyverse)
library(lubridate)
library(tidytext)

twitter<-read_csv("data/twitter.csv")

twitter<-twitter %>%
  mutate(timestamp = ymd_hms(timestamp))

ggplot(twitter, aes(x = timestamp, fill = companyName)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE)

#cleaning the text
remove_reg <- "&amp;|&lt;|&gt;"
tidy_tweets <- twitter %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))
