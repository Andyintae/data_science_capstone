library(tidyverse)
library(tidytext)
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

rm(list=c("twit","blog","news","con"))
gc()

# sampling ------------------------------------------------------------------


# Let's choose 10% of data
set.seed(2020)
docu_samp <- docu %>% group_by(category) %>% sample_frac(size = 0.1, replace = FALSE)
rm(docu)

# 2. Cleaning the data ----------------------------------------------------------

# Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. 
#                Writing a function that takes a file as input and returns a tokenized version of it.
# Profanity filtering - removing profanity and other words you do not want to predict.

# 2-1 cleaning text -------------------------------------------------------

docu_samp_clean <- docu_samp %>% 
  mutate(text = tolower(text),
         text = str_replace_all(text, "???", "'"),        # to use replace_contraction changing ??? to ' (They are differnt characters.)
         text = replace_contraction(text),              # expand contractions
         text = str_replace_all(text, "\\d", ""),       # remove numbers
         text = str_replace_all(text, "[:punct:]", ""), # remove punctuation
         text = replace_white(text)                     # replace one or more white space with a single space
  )

rm(docu_samp)
gc()

# 2-2 language detect ---------------------------------------------------------

docu_samp_clean_eng <- as_tibble(detect_language(as.vector(docu_samp_clean$text)))
names(docu_samp_clean_eng) <- c("lang")

docu_samp_clean <- bind_cols(docu_samp_clean, docu_samp_clean_eng)

docu_samp_set <- docu_samp_clean %>% ungroup() %>%  filter(lang == "en") %>% select(text)

write_rds(docu_samp_set, "docu_samp_set.rds")

# 2-3 create profanity words set
profanity <- as_tibble(c(unique(tolower(lexicon::profanity_alvarez)), profanity_arr_bad, profanity_banned, profanity_zac_anger, profanity_racist))

profanity_set <- profanity %>%
  mutate(value   = tolower(value),
         value   = str_replace_all(value ,"[:punct:]", ""),
         value   = replace_white(value), 
         lexicon = "profanity",
         wd_cnt  = str_count(value, "\\s") + 1,
         len     = nchar(value)) %>% 
  filter(wd_cnt <= 4 & len > 2) %>% 
  select(word = value , lexicon)

write_rds(profanity_set, "profanity_set.rds")