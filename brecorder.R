library(rvest)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(textdata)

# Function to scrape headlines from a given URL and date
scrape_headlines <- function(url, date, css_selector) {
  archive_url <- paste0(url, "archive/", date)
  page <- read_html(archive_url)
  story_titles <- page %>%
    html_nodes(css_selector) %>%
    html_text()
  
  return(story_titles)
}

# URLs and CSS selectors for each website
urls <- c("https://www.brecorder.com/") # Business Recorder
css_selectors <- c("h2") # CSS selector for headlines

# Date range to scrape
start_date <- "2024-09-18"
end_date <- "2024-09-23" 
dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
dates <- format(dates, "%Y-%m-%d")

# Scrape headlines from the website
all_headlines <- lapply(seq_along(urls), function(i) {
  lapply(dates, function(date) {
    scrape_headlines(urls[i], date, css_selectors[i])
  })
})
all_headlines <- unlist(all_headlines)

# Keywords related to economy
economy_keywords <- c("economy", "economic", "GDP", "inflation", "unemployment", "fiscal policy", "monetary policy", 
                      "IMF", "circular debt", "prices", "trade", "exports", "imports", "tariffs", "budget", "deficit", 
                      "surplus", "investment", "stock market", "recession", "growth")

# Filter economy-related headlines
econ_br <- grep(paste(economy_keywords, collapse = "|"), all_headlines, value = TRUE, ignore.case = TRUE)

# Create a tibble of the headlines
economy_tibble <- data.frame(headline = econ_br) %>%
  unnest_tokens(word, headline) %>%
  anti_join(stop_words)

# Perform sentiment analysis using Bing lexicon
sentiment_analysis <- inner_join(get_sentiments("bing"), economy_tibble, by = "word")

# Count word frequencies
word_freq <- count(sentiment_analysis, word, sentiment)

# Separate positive and negative words
positive_words <- word_freq %>% filter(sentiment == "positive")
negative_words <- word_freq %>% filter(sentiment == "negative")

# Create a word cloud with distinct colors for positive and negative words
wordcloud(words = positive_words$word, freq = positive_words$n, 
          min.freq = 1, max.words = 100, random.order = FALSE, 
          colors = brewer.pal(8, "Blues"))

wordcloud(words = negative_words$word, freq = negative_words$n, 
          min.freq = 1, max.words = 100, random.order = FALSE, 
          colors = brewer.pal(8, "Reds"))

# Create a combined word cloud with wordcloud2
combined_word_freq <- word_freq %>%
  mutate(color = ifelse(sentiment == "positive", "green", "red"))

wordcloud2(data = combined_word_freq %>% select(word, n, color),
           size = 0.5, color = combined_word_freq$color, shape = "circle")

# Sentiment score analysis with AFINN
afinn <- get_sentiments("afinn")
sentiment_scores <- sentiment_analysis %>%
  inner_join(afinn, by = "word") %>%
  group_by(word) %>%
  summarise(sentiment_score = sum(value))

# Plot sentiment scores
ggplot(sentiment_scores, aes(x = sentiment_score)) +
  geom_histogram(fill = "skyblue", bins = 20) +
  labs(x = "Sentiment Score", y = "Frequency", title = "Distribution of Sentiment Scores")
