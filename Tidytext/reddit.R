

library(tidyverse)
library(RedditExtractoR)
library(magrittr)



s1 <- reddit_urls(search_terms="S&P 500",
                  subreddit = "investing",
                  sort_by = "new",page_threshold = 10)


s2 <- reddit_urls(search_terms="S&P 500",
                  subreddit = "wallstreetbets",
                  sort_by = "new")



s3 <- reddit_urls(search_terms="Microsoft",
                  subreddit = "investing",
                  sort_by = "new")



s4 <- reddit_urls(search_terms="Microsoft",
                  subreddit = "wallstreetbets",
                  sort_by = "new")



s5 <- reddit_urls(search_terms="MSFT",
                  subreddit = "investing",
                  sort_by = "new")



s6 <- reddit_urls(search_terms="MSFT",
                  subreddit = "wallstreetbets",
                  sort_by = "new")

is.data.frame(s1)

colnames(s1)


s6$title

s6$URL

s6c <- reddit_content(s6$URL, wait_time = 4)
s2c <- reddit_content(s2[17,]$URL,wait_time = 4)
is.data.frame(s6c)
colnames(s6c)

s6c[3,]$URL
s6c[2,]$post_text


s6c_test <- s6c %>%  select(URL, post_text)

colnames(s6c_test) <- c("doc_id", "text")

library(tm)
library(tm.plugin.webmining)
library(rJava)
pacman::p_load(rJava)

s6c_test_ds <- DataframeSource(s6c_test)

s6c_test_corp <- Corpus(s6c_test_ds)


tdm <- TermDocumentMatrix(s6c_test_corp,
                          control = list(removePunctuation = TRUE,
                                         stopwords = TRUE))




