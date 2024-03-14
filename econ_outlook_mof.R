

library(pdftools)
library(stringi)
library(tidyverse)
library(janitor)
library(tidytext)
library(tm)
library(wordcloud)
library(stopwords)
econ_fin <- pdf_text("data/monthly_economic_update.pdf")

econ_fin <- econ_fin|>  tibble(line = 1, text = econ_fin)

save(econ_fin,file="data/econ_fin.RData")
load("data/econ_fin.RData") ## Monetary Policy Document

econ_fin |> unnest_tokens(word, text) |> anti_join(stop_words)  -> econ_finance

econ_finance 

# Perform sentiment analysis
sentiment_analysis <- inner_join(get_sentiments("bing"), econ_finance, by = c("word" = "word"))

# Count word frequencies
word_freq <- count(sentiment_analysis, word)

# Create a word cloud
wordcloud(words = word_freq$word, freq = word_freq$n,
          min.freq = 1, max.words = 100, random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Save the word cloud as an image
png("wordcloud.png", width = 800, height = 600)
wordcloud(words = word_freq$word, freq = word_freq$n, min.freq = 1, max.words = 150, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
dev.off()

# Create a bar graph of word frequency
word_freq_top20 <- head(word_freq[order(-word_freq$n),], 20)  # Select top 20 words
ggplot(word_freq_top20, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() + coord_flip() +
  labs(x = "Word", y = "Frequency", title = "Top 20 Words in by Ministry of Finance Feb-24 Outlook")

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
  labs(x = "Sentiment Score", y = "Frequency", title = "Sentiments about Pakistan Economy Feb-24",
       subtitle = "Sentiments expressed by MoF in Feb-24 are mostly positive",
       caption = "By: Zahid Asghar, dated: 2024-02-29") +
  theme_minimal(base_size = 15)


library(ggplot2)

# Your ggplot code
gg <- ggplot(sentiment_scores, aes(x = sentiment_score)) +
  geom_histogram(fill = "steelblue", bins = 15) +
  labs(x = "Sentiment Score", y = "Frequency", title = "Distribution of Sentiment Scores from Headlines",
       subtitle = "Dawn, The News, Tribune, The Nation",
       caption = "By: Zahid Asghar, dated: 2024-02-28") +
  theme_minimal(base_size = 15) +
  theme(plot.background = element_rect(fill = "white"))  # Set background color to white

# Save the ggplot as an image file
ggsave("sentiment_scores_histogram.png", plot = gg, width = 10, height = 6, dpi = 300)



econ_fin_0523 <- pdf_text("data/monthly_economic_update_may24.pdf")

econ_fin_0523 <- econ_fin_0523|>  tibble(line = 1, text = econ_fin_0523)

save(econ_fin_0523,file="data/econ_fin_0523.RData")
load("data/econ_fin_0523.RData") ## Monetary Policy Document

econ_fin_0523 |> unnest_tokens(word, text) |> anti_join(stop_words)  -> econ_finance_0523

econ_finance_0523

# Perform sentiment analysis
sentiment_analysis <- inner_join(get_sentiments("bing"), econ_finance_0523, by = c("word" = "word"))

# Count word frequencies
word_freq <- count(sentiment_analysis, word)

# Create a word cloud
wordcloud(words = word_freq$word, freq = word_freq$n,
          min.freq = 1, max.words = 100, random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Save the word cloud as an image
png("wordcloud.png", width = 800, height = 600)
wordcloud(words = word_freq$word, freq = word_freq$n, min.freq = 1, max.words = 150, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
dev.off()

# Create a bar graph of word frequency
word_freq_top20 <- head(word_freq[order(-word_freq$n),], 20)  # Select top 20 words
ggplot(word_freq_top20, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() + coord_flip() +
  labs(x = "Word", y = "Frequency", title = "Top 20 Words in by Ministry of Finance May-23 Outlook")

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
  labs(x = "Sentiment Score", y = "Frequency", title = "Sentiments about Pakistan Economy May-23",
       subtitle = "Sentiments expressed by MoF in May-23 are well mixed",
       caption = "By: Zahid Asghar, dated: 2024-02-29") +
  theme_minimal(base_size = 15)


library(ggplot2)

# Your ggplot code
gg <- ggplot(sentiment_scores, aes(x = sentiment_score)) +
  geom_histogram(fill = "steelblue", bins = 15) +
  labs(x = "Sentiment Score", y = "Frequency", title = "Distribution of Sentiment Scores from Headlines",
       subtitle = "Dawn, The News, Tribune, The Nation",
       caption = "By: Zahid Asghar, dated: 2024-02-28") +
  theme_minimal(base_size = 15) +
  theme(plot.background = element_rect(fill = "white"))  # Set background color to white

# Save the ggplot as an image file
ggsave("sentiment_scores_histogram.png", plot = gg, width = 10, height = 6, dpi = 300)



