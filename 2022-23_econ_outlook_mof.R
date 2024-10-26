

library(pdftools)



library(stringi)
library(tidyverse)
library(janitor)
library(tidytext)
library(tm)
library(wordcloud)
library(stopwords)
pk_eco_survey <- pdf_text("data/2022_23_Overview_of_the_Economy.pdf")

pk_eco_survey <- pk_eco_survey|>  tibble(line = 1, text = pk_eco_survey)

save(pk_eco_survey,file="data/pk_eco_survey.RData")
load("data/pk_eco_survey.RData") ## Monetary Policy Document

pk_eco_survey|> unnest_tokens(word, text) |> anti_join(stop_words)  -> pak_eco_23



# Perform sentiment analysis
sentiment_analysis <- inner_join(get_sentiments("bing"), pak_eco_23, by = c("word" = "word"))


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


# Save the ggplot as an image file
ggsave("sentiment_scores_histogram.png", plot = gg, width = 10, height = 6, dpi = 300)



pk_eco_survey_2324 <- pdf_text("data/overview_2023-24.pdf")

pk_eco_survey_2324 <- pk_eco_survey_2324|>  tibble(line = 1, text = pk_eco_survey_2324)

save(pk_eco_survey_2324,file="data/pk_eco_survey_2324.RData")
load("data/pk_eco_survey_2324.RData") ## Monetary Policy Document

pk_eco_survey_2324|> unnest_tokens(word, text) |> anti_join(stop_words)  -> pk_eco_24



# Perform sentiment analysis
sent_analysis_24 <- inner_join(get_sentiments("bing"), pk_eco_24, by = c("word" = "word"))


# Count word frequencies
word_freq <- count(sent_analysis_24, word)

# Create a word cloud
wordcloud(words = word_freq$word, freq = word_freq$n,
          min.freq = 1, max.words = 100, random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Save the word cloud as an image
png("wordcloud_24.png", width = 900, height = 1000)
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
sentiment_scores <- sent_analysis_24 %>%
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


# Save the ggplot as an image file
ggsave("sentiment_scores_histogram.png", plot = gg, width = 10, height = 6, dpi = 300)



