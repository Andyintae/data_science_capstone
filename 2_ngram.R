library(tidyverse)
library(tidytext)
library(tm)
library(NLP)
library(textclean)
library(lexicon)
library(cld3)

docu_samp_set <- readRDS("docu_samp_set.rds")
profanity_set <- readRDS("profanity_set.rds")

# 2-1. Unigram -----------------------------------------------------------------

docu_samp_uni <- docu_samp_set %>% 
  unnest_tokens(text, text, token = "ngrams", n = 1)

docu_samp_uni_cnt <- docu_samp_uni %>% 
  filter(!text %in% stop_words$word) %>% 
  filter(!text %in% profanity_set$word) %>% 
  count(text, sort = TRUE) %>% 
  mutate(text = fct_reorder(text, n))

dic_uni <- docu_samp_uni_cnt %>% mutate(prop = n/sum(n))


write_rds(dic_uni, "dic_uni.rds")
gc()


# bigram ------------------------------------------------------------------
docu_samp_bigm <- docu_samp_set %>% 
  unnest_tokens(text, text, token = "ngrams", n = 2)

docu_samp_bigm_sep <- docu_samp_bigm %>% 
  separate(text, into = c("word1", "word2"), remove = FALSE, sep = "\\s")

dic_bi <- docu_samp_bigm_sep %>% 
  filter(!text  %in% profanity_set$word & 
         !word1 %in% profanity_set$word & 
         !word2 %in% profanity_set$word &
         !word2 %in% stop_words$word) %>% 
  count(text, sort = TRUE) %>% 
  mutate(text = fct_reorder(text, n),
         prop = n/sum(n))

write_rds(dic_bi, "dic_bi.rds")
gc()

# trigram -----------------------------------------------------------------
docu_samp_trgm <- docu_samp_set %>% 
  unnest_tokens(text, text, token = "ngrams", n = 3)

docu_samp_trgm_sep <- docu_samp_trgm %>% 
  separate(text, into = c("word1", "word2", "word3"), remove = FALSE, sep = "\\s")

dic_tri <- docu_samp_trgm_sep %>% 
  filter(!text  %in% profanity_set$word & 
         !word1 %in% profanity_set$word & 
         !word2 %in% profanity_set$word &
         !word3 %in% profanity_set$word &
         !word3 %in% stop_words$word) %>% 
  count(text, sort = TRUE) %>% 
  mutate(text = fct_reorder(text, n),
         prop = n/sum(n)
         )

write_rds(dic_tri, "dic_tri.rds")
rm(list = c("docu_samp_trgm","docu_samp_trgm_sep"))
gc()


# make dictionary ---------------------------------------------------------

dic_tr_samp_vec <- as.vector(dic_tri$text[1:100000])
dic_bi_samp_vec <- as.vector(dic_bi$text[1:100000])
dic_un_samp_vec <- as.vector(dic_uni$text[1:1])

write_rds(dic_tr_samp_vec, "dic_tr_samp_vec.rds")
write_rds(dic_bi_samp_vec, "dic_bi_samp_vec.rds")
write_rds(dic_un_samp_vec, "dic_un_samp_vec.rds")

dic_tr_dic <- as_tibble(dic_tri[1:100000,])
dic_bi_dic <- as_tibble(dic_bi[1:100000,])
dic_un_dic <- as_tibble(dic_uni[1:100000,])

dic_ngram <- bind_rows("trigram" = dic_tr_dic, "bigram" = dic_bi_dic, "unigram" = dic_un_dic, .id = "category")
names(dic_ngram) <- c("category", "text", "number_of_text", "proportion")

write_rds(dic_ngram, "dic_ngram.rds")
