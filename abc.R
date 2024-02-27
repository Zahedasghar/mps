#https://www.r-bloggers.com/2020/06/news-headlines-text-analysis/#google_vignette


suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(textdata))
#install.packages("widyr")
suppressPackageStartupMessages(library(widyr))
suppressPackageStartupMessages(library(ggplot2))


packages <- c("stringr", "dplyr", "tidytext", "tidyr", "textdata", "widyr", "ggplot2","readr")
version <- lapply(packages, packageVersion)
version_c <- do.call(c, version)
data.frame(packages=packages, version = as.character(version_c))

get_sentiments("nrc")
get_sentiments("bing")
get_sentiments("afinn")
#get_sentiments("loughran")

news_data <- read.csv("data/archive/abcnews-date-text.csv", header = TRUE, stringsAsFactors = FALSE)
dim(news_data)
head(news_data)


news_df <- news_data %>% select(headline_text)

news_tokens <- news_df %>% unnest_tokens(word, headline_text) 

head(news_tokens, 10)


news_tokens_count <- news_tokens %>% count(word, sort = TRUE) %>% mutate(proportion = n / sum(n))

head(news_tokens_count, 10)

data(stop_words)
head(stop_words, 10)


news_tokens_no_sp <- news_tokens %>% anti_join(stop_words)
head(news_tokens_no_sp, 10)

news_tokens_count <- news_tokens_no_sp %>% count(word, sort = TRUE) %>% mutate(proportion = n / sum(n))
head(news_tokens_count, 10)


news_token_over8000 <- news_tokens_count %>% filter(n > 8000) %>% mutate(word = reorder(word, n))

nrow(news_token_over8000)

head(news_token_over8000, 10)

tail(news_token_over8000, 10)


news_token_over8000 %>%  
  ggplot(aes(word, proportion*1000, fill=ceiling(proportion*1000))) +
  geom_col() + xlab(NULL) + coord_flip() + theme(legend.position = "none")

head(news_df, 10)


news_df_subset <- news_df[1:1000,,drop=FALSE]
tkn_l <- apply(news_df_subset, 1, function(x) { data.frame(headline_text=x, stringsAsFactors = FALSE) %>% unnest_tokens(word, headline_text)})

single_news_tokens <- lapply(tkn_l, function(x) {anti_join(x, stop_words)})

str(single_news_tokens, list.len = 5)

single_news_tokens[[1]]


compute_sentiment <- function(d) {
  if (nrow(d) == 0) {
    return(NA)
  }
  neg_score <- d %>% filter(sentiment=="negative") %>% nrow()
  pos_score <- d %>% filter(sentiment=="positive") %>% nrow()
  pos_score - neg_score
} 

sentiments_bing <- get_sentiments("bing")
str(sentiments_bing)
## Classes 'tbl_df', 'tbl' and 'data.frame':    6786 obs. of  2 variables:
##  $ word     : chr  "2-faces" "abnormal" "abolish" "abominable" ...
##  $ sentiment: chr  "negative" "negative" "negative" "negative" ...

single_news_sentiment_bing <- sapply(single_news_tokens, function(x) { x %>% inner_join(sentiments_bing) %>% compute_sentiment()})

str(single_news_sentiment_bing)

summary(single_news_sentiment_bing)


single_news_sentiment_bing_df <- data.frame(headline_text=news_df_subset$headline_text, score = single_news_sentiment_bing)
head(single_news_sentiment_bing_df, 10)

sentiments_nrc <- get_sentiments("nrc")
(unique_sentiments_nrc <- unique(sentiments_nrc$sentiment))
