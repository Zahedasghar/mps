# Load necessary libraries
library(pdftools)
library(tidyverse)
library(janitor)
library(tidytext)
library(tm)
library(wordcloud)
library(wordcloud2)
library(stopwords)
library(ggplot2)

# Load and process the PDF for 2023-24
pk_eco_survey_2324 <- pdf_text("data/pes_23_24.pdf")

# Convert the PDF text into a tibble for processing
pk_eco_survey_2324 <- pk_eco_survey_2324 |> 
  tibble(line = 1, text = pk_eco_survey_2324)

# Save the extracted data for future use (optional)
save(pk_eco_survey_2324, file = "data/pk_eco_survey_2324.RData")
load("data/pk_eco_survey_2324.RData") # Load if needed later

# Clean and tokenize the text, removing stopwords
pk_eco_survey_clean_2324 <- pk_eco_survey_2324 |> 
  unnest_tokens(word, text) |> 
  anti_join(stop_words)

# Perform sentiment analysis using the Bing lexicon
sentiment_analysis_2324 <- inner_join(get_sentiments("bing"), pk_eco_survey_clean_2324, by = "word")

# Count word frequencies
word_freq_2324 <- count(sentiment_analysis_2324, word)

# Create a word cloud for 2023-24
wordcloud(words = word_freq_2324$word, freq = word_freq_2324$n,
          min.freq = 1, max.words = 100, random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Save the word cloud as an image
png("wordcloud_2324.png", width = 800, height = 600)
wordcloud(words = word_freq_2324$word, freq = word_freq_2324$n, min.freq = 1, max.words = 150, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
dev.off()

# Create an interactive word cloud using wordcloud2
wordcloud2(data = word_freq_2324, size = 0.5, shape = "circle")

# Create a bar graph of top 20 word frequencies
word_freq_top20_2324 <- head(word_freq_2324[order(-word_freq_2324$n),], 20)
ggplot(word_freq_top20_2324, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() + coord_flip() +
  labs(x = "Word", y = "Frequency", 
       title = "Top 20 Words in Pakistan Economic Survey 2023-24 Highlights")

# Save the bar graph as an image file
ggsave("top20_word_frequency_2324.png", width = 10, height = 6, dpi = 300)

# Get AFINN lexicon for sentiment scoring
afinn <- get_sentiments("afinn")

# Compute sentiment scores using AFINN
sentiment_scores_2324 <- sentiment_analysis_2324 %>%
  inner_join(afinn, by = "word") %>%
  group_by(word) %>%
  summarise(sentiment_score = sum(value, na.rm = TRUE))

# Plot sentiment score distribution
ggplot(sentiment_scores_2324, aes(x = sentiment_score)) +
  geom_histogram(fill = "steelblue", bins = 15) +
  labs(x = "Sentiment Score", y = "Frequency", 
       title = "Sentiments about Pakistan Economy in 2023-24 Highlights",
       subtitle = "Sentiments expressed in the document",
       caption = "Source: Pakistan Economic Survey 2023-24") +
  theme_minimal(base_size = 15)

# Save the sentiment score histogram as an image
ggsave("sentiment_scores_histogram_2324.png", width = 10, height = 6, dpi = 300)

# Complete analysis for 2023-24 data
