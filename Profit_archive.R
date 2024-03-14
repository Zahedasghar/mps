library(rvest)
# Specify the URL of the Tribune Pakistan website
url <- "https://profit.pakistantoday.com.pk/category/headlines/"

# Read the HTML content of the webpage
page <- read_html(url)

story_titles_profit <- page %>%
  html_nodes("h3") %>%  # Try a different CSS selector
  html_text()

# Print the extracted story titles
print(story_titles_profit)

# Keywords related to economy
economy_keywords <- c("economy", "economic", "GDP", "inflation", "unemployment", "fiscal policy", "monetary policy", 
                      "circular debt","prices","inflation", "trade", "exports", "imports", "tariffs", "budget", "deficit", "surplus", "investment", 
                      "stock market", "recession", "growth")


# Filter story titles containing economy-related keywords
economy_related_titles <- grep(paste(economy_keywords, collapse = "|"), story_titles_profit, value = TRUE, ignore.case = TRUE)

# Print the economy-related titles
print(economy_related_titles)



economy_tibble <- data.frame(headline = economy_related_titles) %>%
  unnest_tokens(word, headline)

economy_tibble |> dim()
economy_tibble <- economy_tibble %>%
  anti_join(stop_words)






# Perform sentiment analysis
sentiment_analysis <- inner_join(get_sentiments("bing"), economy_tibble, by = c("word" = "word"))

# Count word frequencies
word_freq <- count(sentiment_analysis, word)

# Create a word cloud
wordcloud(words = word_freq$word, freq = word_freq$n,
          min.freq = 1, max.words = 10, random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Create a bar graph of word frequency
word_freq <- count(sentiment_analysis, word, sort = TRUE)
word_freq <- head(word_freq, 20)  # Select top 20 words
ggplot(word_freq, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() + coord_flip() +
  labs(x = "Word", y = "Frequency", title = "Top 20 Words in Economy-related Headlines")

library(textdata)
# Get AFINN lexicon
afinn <- get_sentiments("afinn")

sentiment_scores <- sentiment_analysis %>%
  inner_join(afinn, by = c("word" = "word")) %>%
  group_by(word) %>%
  summarise(sentiment_score = sum(value, na.rm = TRUE))  # Use `value` instead of `afinn_score`
# Plot sentiment scores
ggplot(sentiment_scores, aes(x = sentiment_score)) +
  geom_histogram(fill = "skyblue", bins = 15) +
  labs(x = "Sentiment Score", y = "Frequency", title = "Distribution of Sentiment Scores")






