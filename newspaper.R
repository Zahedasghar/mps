library(rvest)

# Function to scrape headlines from a given URL
scrape_headlines <- function(url, css_selector) {
  # Read the HTML content of the webpage
  page <- read_html(url)
  
  # Extract story headlines using CSS selectors
  story_titles <- page %>%
    html_nodes(css_selector) %>% 
    html_text()
  
  return(story_titles)
}

# Scrape headlines from Dawn
dawn_url <- "https://www.dawn.com/"
dawn_headlines <- scrape_headlines(dawn_url, ".story__title")

# Scrape headlines from The News International
news_url <- "https://www.thenews.com.pk/"
news_headlines <- scrape_headlines(news_url, "h2")

# Scrape headlines from The Express Tribune
tribune_url <- "https://www.tribune.com.pk/"
tribune_headlines <- scrape_headlines(tribune_url, "h3")

# Scrape headlines from The Nation
nation_url <- "https://nation.com.pk/"
nation_headlines <- scrape_headlines(nation_url, "h3")

# Combine all headlines into one vector
all_headlines <- c(dawn_headlines, news_headlines, tribune_headlines, nation_headlines)

# Print the first few headlines from the combined list
print(head(all_headlines))




library(tidytext)
library(dplyr)
library(wordcloud)
library(tm)
library(SnowballC)
library(textdata)
library(ggplot2)

# Create a dataframe with headlines
headlines_df <- data.frame(headline = all_headlines)

# Tokenize the headlines
headlines_tokenized <- headlines_df %>%
  unnest_tokens(word, headline)

# Remove stop words
headlines_cleaned <- headlines_tokenized %>%
  anti_join(stop_words)

# Perform sentiment analysis
sentiment_analysis <- inner_join(get_sentiments("bing"), headlines_cleaned, by = c("word" = "word"))

# Count word frequencies
word_freq <- count(sentiment_analysis, word)

# Create a word cloud
wordcloud(words = word_freq$word, freq = word_freq$n,
          min.freq = 1, max.words = 100, random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Create a bar graph of word frequency
word_freq_top20 <- head(word_freq[order(-word_freq$n),], 20)  # Select top 20 words
ggplot(word_freq_top20, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() + coord_flip() +
  labs(x = "Word", y = "Frequency", title = "Top 20 Words in Headlines")

# Get AFINN lexicon
afinn <- get_sentiments("afinn")

# Compute sentiment scores
sentiment_scores <- sentiment_analysis %>%
  inner_join(afinn, by = c("word" = "word")) %>%
  group_by(word) %>%
  summarise(sentiment_score = sum(value, na.rm = TRUE))

# Plot sentiment scores
ggplot(sentiment_scores, aes(x = sentiment_score)) +
  geom_histogram(fill = "steelblue", bins = 15) +
  labs(x = "Sentiment Score", y = "Frequency", title = "Distribution of Sentiment Scores from Headlines",
       subtitle = "Dawn, The News, Tribune, The Nation",
       caption = "By: Zahid Asghar, dated: 2024-02-26") +
  theme_minimal(base_size = 15)


library(ggplot2)

# Your ggplot code
gg <- ggplot(sentiment_scores, aes(x = sentiment_score)) +
  geom_histogram(fill = "steelblue", bins = 15) +
  labs(x = "Sentiment Score", y = "Frequency", title = "Distribution of Sentiment Scores from Headlines",
       subtitle = "Dawn, The News, Tribune, The Nation",
       caption = "By: Zahid Asghar, dated: 2024-02-26") +
  theme_minimal(base_size = 15) +
  theme(plot.background = element_rect(fill = "white"))  # Set background color to white

# Save the ggplot as an image file
ggsave("sentiment_scores_histogram.png", plot = gg, width = 10, height = 6, dpi = 300)

