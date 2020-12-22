library(tidyverse)
library(readr)
library(profr)
library(tm)
library(NLP)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(stringr)
library(tidytext)

object.size()
Rprof()
gc()

# load files --------------------------------------------------------------

setwd("final/en_US")

en_twit <- readLines(con <- file("en_US.twitter.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)

en_blog <- readLines(con <- file("en_US.blogs.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)

en_news <- readLines(con <- file("en_US.news.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)


class(en_news)
length(en_news)
en_news[1]


# wordcloud ---------------------------------------------------------------


wordcloud(en_news, random.order = TRUE)



# tibble ------------------------------------------------------------------


en_blog_tb <- as_tibble(en_blog)

en_blog_wd <- en_blog_tb %>% 
  unnest_tokens(word, value) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  ungroup() %>% 
  mutate(word = fct_reorder(word, n))

en_blog_wd %>% top_n(10) %>% 
  ggplot(aes(x = word, y = n, fill = word))+
  geom_col()+
  coord_flip()+
  labs(title = " Top 10 words in en_US.blogs", x = "Word", y = "Count")

log(4/3)*0.25