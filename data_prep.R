library(tidyverse)
library(tidytext)
library(markovchain)
library(tm)
library(NLP)
library(textclean)
library(lexicon)
library(sentimentr)




# 1. Getting the data -----------------------------------------------

if (!file.exists("capstone.zip")){
  download.file(url = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", destfile = "capstone.zip")
  unzip("capstone.zip")
}

setwd("final/en_US")

con <- file("en_US.twitter.txt", "r")
twit <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

con <- file("en_US.blogs.txt", "r")
blog <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

con <- file("en_US.news.txt", "r")
news <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

  # Transfoming the data to tibble to handle them tidyverse and tidytext package
twit <- as_tibble(twit)
blog <- as_tibble(blog)
news <- as_tibble(news)

docu <- bind_rows("twitter" = twit, "blogs" = blog, "news" = news, .id = "category")
names(docu) <- c("category", "text")

write_rds(docu, "docu.rds")
rm(list=c("twit","blog","news","con"))


  # Making sample data set cosidering system resource

docu <- readRDS("docu.rds")

  # Let's choose 20% of data
set.seed(2020)
docu_samp <- docu %>% group_by(category) %>% sample_frac(size = 0.01, replace = FALSE)
rm(docu)


# 2. Cleaning the data ----------------------------------------------------------
 
# Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. 
#                Writing a function that takes a file as input and returns a tokenized version of it.
# Profanity filtering - removing profanity and other words you do not want to predict.



# 2-1 cleaning text -------------------------------------------------------

docu_samp_clean <- docu_samp %>% 
    mutate(text = tolower(text),
           text = str_replace_all(text, "’", "'"),        # to use replace_contraction
           text = replace_contraction(text),              # expand contractions
           text = replace_white(text),                    # replace one or more white space with a single space
           text = str_replace_all(text, "\\d", ""),       # remove numbers
           text = str_replace_all(text, "[:punct:]", "")  # remove punctuation
           )

  # add profanity words in stop_words (We will use in next step)
profanity_set <- as_tibble(c(profanity_alvarez, profanity_arr_bad, profanity_banned, profanity_zac_anger, profanity_racist))
profanity_add <- profanity_set %>% mutate(lexicon = "profanity") %>% select(word = value , lexicon)
stop_words_profa <- bind_rows(stop_words, profanity_add)

# 2-1. Unigram -----------------------------------------------------------------

docu_samp_uni <- docu_samp_clean %>% 
  ungroup() %>% 
  select(text) %>% 
  unnest_tokens(unigram, text, token = "ngrams", n = 1)

docu_samp_uni_cnt <- docu_samp_uni %>% 
  filter(!unigram %in% stop_words$word) %>% 
  count(bigram, sort = TRUE) %>% 
  mutate(bigram = fct_reorder(bigram, n))

text_mod <- markovchainFit(dcmt_samp_bigm_sp_cnt$bigram)







           



# 3. Exploratory Data Analysis --------------------------------------------



# bigram ------------------------------------------------------------------
dcmt_samp_bigm <- dcmt_samp %>% 
  group_by(category) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

dcmt_samp_bigm_sp <- dcmt_samp_bigm %>% 
  separate(bigram, into = c("word1", "word2"), remove = FALSE, sep = "\\s")

rm(dcmt_samp_bigm)
gc()

dcmt_samp_bigm_sp_cnt <- dcmt_samp_bigm_sp %>% 
  filter(!word1 %in% stop_words$word & 
           !word2 %in% stop_words$word) %>% 
  count(bigram, sort = TRUE) %>% 
  mutate(bigram = fct_reorder(bigram, n))

text_mod <- markovchainFit(dcmt_samp_bigm_sp_cnt$bigram)

# trigram -----------------------------------------------------------------
dcmt_samp_trgm <- dcmt_samp %>% 
  group_by(category) %>% 
  unnest_tokens(trgram, text, token = "ngrams", n = 3)

dcmt_samp_trgm_sp <- dcmt_samp_trgm %>% 
  separate(trgram, into = c("word1", "word2", "word3"), remove = FALSE, sep = "\\s")

rm(dcmt_samp_trgm)
gc()

dcmt_samp_trgm_sp_cnt <- dcmt_samp_trgm_sp %>% 
  filter(!word1 %in% stop_words$word & 
           !word2 %in% stop_words$word & 
           !word3 %in% stop_words$word) %>% 
  count(trgram, sort = TRUE) %>% 
  mutate(trgram2 = fct_reorder(trgram, n))

# quadgram ----------------------------------------------------------------
dcmt_samp_trgm <- dcmt_samp %>% 
  group_by(category) %>% 
  unnest_tokens(trgram, text, token = "ngrams", n = 3)

dcmt_samp_trgm_sp <- dcmt_samp_trgm %>% 
  separate(trgram, into = c("word1", "word2", "word3"), remove = FALSE, sep = "\\s")

rm(dcmt_samp_trgm)
gc()

dcmt_samp_trgm_sp_cnt <- dcmt_samp_trgm_sp %>% 
  filter(!word1 %in% stop_words$word & 
           !word2 %in% stop_words$word & 
           !word3 %in% stop_words$word) %>% 
  count(trgram, sort = TRUE) %>% 
  mutate(trgram2 = fct_reorder(trgram, n))





