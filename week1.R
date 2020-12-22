
library(tidyverse)
library(readxl)
library(readr)

download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", destfile = "capstone.zip")

unzip("capstone.zip")


# 1. size -----------------------------------------------------------------

info_en_bl <- file.info("en_US.blogs.txt")
info_en_bl$size/1024/1000



# load files --------------------------------------------------------------


setwd("final/en_US")

en_twit <- readLines(con <- file("en_US.twitter.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)

en_blog <- readLines(con <- file("en_US.blogs.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)

en_news <- readLines(con <- file("en_US.news.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)


# length ------------------------------------------------------------------

max(nchar(en_twit))
max(nchar(en_blog))
max(nchar(en_news))

love_count <- sum(grepl("love", twitter))
hate_count <- sum(grepl("hate", twitter))
love_count / hate_count

biostats <- grep("biostats", twitter)
twitter[biostats]

tw_tx <- "A computer once beat me at chess, but it was no match for me at kickboxing"
sum(grepl(tw_tx, twitter))