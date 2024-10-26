# Load necessary libraries
library(doParallel)
library(rvest)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(tm)
library(SnowballC)
library(textdata)

# Set up parallel processing
all_cores <- parallel::detectCores(logical = FALSE) - 1
registerDoParallel(all_cores)

# Function to scrape headlines from a given URL
scrape_headlines <- function(url, css_selector) {
  page <- read_html(url)
  story_titles <- page %>%
    html_nodes(css_selector) %>%
    html_text()
  
  return(story_titles)
}

# URLs and CSS selectors for each website
urls <- c(
  "https://www.dawn.com/",           # Dawn
  "https://www.thenews.com.pk/",     # The News
  "https://www.brecorder.com/"       # Business Recorder
)

css_selectors <- c(
  ".story__title",  # Dawn
  "h2",             # The News
  "h2"              # Business Recorder
)

# Scrape headlines from each website
all_headlines <- lapply(seq_along(urls), function(i) {
  scrape_headlines(urls[i], css_selectors[i])
})
all_headlines <- unlist(all_headlines)

# Filter story titles containing economy-related keywords
economy_keywords <- c("economy", "economic", "GDP", "inflation", "unemployment", "fiscal policy", 
                      "monetary policy", "circular debt", "prices", "trade", "exports", "imports", 
                      "tariffs", "budget", "deficit", "surplus", "investment", "stock market", 
                      "recession", "growth")

economy_related_titles <- grep(paste(economy_keywords, collapse = "|"), all_headlines, value = TRUE, ignore.case = TRUE)

# Create a tibble for sentiment analysis
word_tibble <- data.frame(headline = economy_related_titles) %>%
  unnest_tokens(word, headline) %>%
  anti_join(stop_words)

# Perform sentiment analysis using Bing lexicon
sentiment_analysis <- inner_join(get_sentiments("bing"), word_tibble, by = "word")

# Count word frequencies for visualization
word_freq <- count(sentiment_analysis, word, sort = TRUE)

# Create a word cloud with more words
wordcloud(words = word_freq$word, freq = word_freq$n,
          min.freq = 1,  # Lowering the minimum frequency to 1 to capture more words
          max.words = 100,  # Increasing the max words to show more
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Create an interactive word cloud using wordcloud2
word_freq_for_wc2 <- word_freq %>% 
  select(word, n)  # Select only word and frequency columns for wordcloud2

wordcloud2(data = word_freq_for_wc2, size = 0.5, shape = "circle")

# Create a bar graph of top 20 most frequent words
top_word_freq <- head(word_freq, 20)  # Select top 20 words
ggplot(top_word_freq, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() + coord_flip() +
  labs(x = "Word", y = "Frequency", title = "Top 20 Words in Economy-related Headlines")

# Get AFINN lexicon for sentiment scoring
afinn <- get_sentiments("afinn")

# Perform sentiment scoring with AFINN lexicon
sentiment_scores <- sentiment_analysis %>%
  inner_join(afinn, by = "word") %>%
  group_by(word) %>%
  summarise(sentiment_score = sum(value, na.rm = TRUE)) 

# Filter extreme sentiment scores for better visualization
sentiment_scores <- sentiment_scores |> filter(sentiment_score > -90 & sentiment_score < 25)

# Plot distribution of sentiment scores
ggplot(sentiment_scores, aes(x = sentiment_score)) +
  geom_histogram(fill = "skyblue", bins = 15) +
  labs(x = "Sentiment Score", y = "Frequency", title = "Distribution of Sentiment Scores")

# Stop the parallel cluster
doParallel::stopImplicitCluster()
