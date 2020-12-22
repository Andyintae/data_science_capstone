library(tidyverse)
library(tidytext)
# library(tm)
# library(NLP)
# library(SnowballC)
# library(RColorBrewer)
# library(wordcloud)


# Download and read files -------------------------------------------------

download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", destfile = "capstone.zip")
unzip("capstone.zip")
setwd("final/en_US")

en_twit <- readLines(connect1 <- file("en_US.twitter.txt"), encoding = "UTF-8", skipNul = TRUE)
close(connect1)

en_blog <- readLines(connect2 <- file("en_US.blogs.txt"), encoding = "UTF-8", skipNul = TRUE)
close(connect2)

en_news <- readLines(connect3 <- file("en_US.news.txt"), encoding = "UTF-8", skipNul = TRUE)
close(connect3)

gc()
# prepare for analysis ----------------------------------------------------

en_twit <- as_tibble(en_twit)
en_blog <- as_tibble(en_blog)
en_news <- as_tibble(en_news)

dcmt <- bind_rows("twitter" = en_twit, "blogs" = en_blog, "news" = en_news, .id = "category")
names(dcmt) <- c("category", "text")

# check the data set ---------------------------------------------------------------------

glimpse(dcmt)
str(dcmt)
  # There is one charater variable in each data set.

dcmt %>% group_by(category) %>% summarise(count = n(), max_length = max(nchar(text)))
  # the length of the longest line seen in the blogs data set : over 40 thousand

sample_
# Build tidy word set -----------------------------------------------------

  # make sample date set
set.seed(1222)
dcmt_samp <- dcmt %>% group_by(category) %>% sample_n(size = 20000, replace = FALSE)

  # Count word in each data set

dcmt_samp %>% 
  group_by(category) %>% 
  unnest_tokens(word, text) %>% 
  count(category, sort = TRUE)

dcmt_samp_wd <- dcmt_samp %>% 
  group_by(category) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(category, word, sort = TRUE) %>% 
  mutate(word2 = fct_reorder(word, n))

dcmt_samp_wd %>% top_n(10)


# make plots of word count ------------------------------------------------

dcmt_samp_wd %>% 
  group_by(category) %>% 
  top_n(10) %>% 
  ggplot(aes(x = word2, y = n, fill = category))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = " Top 10 words", x = "Word", y = "Count")+
  facet_wrap(~category, scales = "free")



# bi-grams -----------------------------------------------------------------

dcmt_samp %>% 
  group_by(category) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(category)

dcmt_samp_bigm <- dcmt_samp %>% 
  group_by(category) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) 

dcmt_samp_bigm_sp <- dcmt_samp_bigm %>% 
  separate(bigram, into = c("word1", "word2"), remove = FALSE, sep = "\\s")

dcmt_samp_bigm_sp_cnt <- dcmt_samp_bigm_sp %>% 
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>% 
  count(bigram, sort = TRUE) %>% 
  mutate(bigram2 = fct_reorder(bigram, n))

dcmt_samp_bigm_sp_cnt %>% 
  group_by(category) %>% 
  top_n(10) %>% 
  ggplot(aes(x = bigram2, y = n, fill = category))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = " Top 10 Bi-grams", x = "Bi-gram", y = "Count")+
  facet_wrap(~category, scales = "free")


# bi-grams -----------------------------------------------------------------

dcmt_samp_trgm <- dcmt_samp %>% 
  group_by(category) %>% 
  unnest_tokens(trgram, text, token = "ngrams", n = 3)

dcmt_samp_trgm_sp <- dcmt_samp_trgm %>% 
  separate(trgram, into = c("word1", "word2", "word3"), remove = FALSE, sep = "\\s")

dcmt_samp_trgm_sp_cnt <- dcmt_samp_trgm_sp %>% 
  filter(!word1 %in% stop_words$word & 
         !word2 %in% stop_words$word & 
         !word3 %in% stop_words$word) %>% 
  count(trgram, sort = TRUE) %>% 
  mutate(trgram2 = fct_reorder(trgram, n))

dcmt_samp_trgm_sp_cnt %>% 
  group_by(category) %>% 
  top_n(10) %>% 
  ggplot(aes(x = trgram2, y = n, fill = category))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = " Top 10 Tri-grams", x = "Tri-gram", y = "Count")+
  facet_wrap(~category, scales = "free")



