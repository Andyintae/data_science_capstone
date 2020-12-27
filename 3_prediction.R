library(tidyverse)
library(tidytext)
library(tm)
library(NLP)
library(textclean)
library(lexicon)
library(cld3)


dic_tr_samp_vec <- readRDS("dic_tr_samp_vec.rds")
dic_bi_samp_vec <- readRDS("dic_bi_samp_vec.rds")
dic_un_samp_vec <- readRDS("dic_un_samp_vec.rds")

# prediction --------------------------------------------------------------

text_cln <- function(x){
  x <- tolower(x)
  x <- str_replace_all(x, "???", "'")
  x <- replace_contraction(x)
  x <- str_replace_all(x, pattern = "\\d", replacement = "")
  x <- str_replace_all(x, pattern = "[:punct:]", replacement = "")
  x <- replace_white(x)
  x
}

# input sample
in_text <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"

# clean the input text
cln_text <- as_tibble(text_cln(in_text))
in_text_spl <- str_split(cln_text, " ")[[1]]

text_len <- length(in_text_spl)

# input_qd  <- paste(tail(in_text_spl, 3), collapse = " ")
input_tr <- paste(tail(in_text_spl, 2), collapse = " ")
input_bi <- paste(tail(in_text_spl, 1), collapse = " ")


# detect the text

tr <- dic_tr_samp_vec[grep(paste0("^",input_tr), dic_tr_samp_vec)[1]]
bi <- dic_bi_samp_vec[grep(paste0("^",input_bi), dic_bi_samp_vec)[1]]
un <- dic_un_samp_vec[1]

result <- ifelse(text_len >=2, (ifelse(!is.na(tr),tr, ifelse(!is.na(bi),bi,un))),
       ifelse(text_len %in% c(1,2), (ifelse(!is.na(bi),bi,un)), NA))



answer <- paste(in_text, tail(str_split(result, " ")[[1]], 1))

answer






