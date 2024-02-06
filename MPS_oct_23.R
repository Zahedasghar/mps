library(pdftools)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(ggwordcloud)
library(ggplotify)

# Download and save the PDF file
url <- "https://www.sbp.org.pk/m_policy/2023/MPS-Oct-2023-Eng.pdf"
pdf_file <- "MPS-Sep-2023-Eng.pdf"
download.file(url, pdf_file, mode = "wb")

# Extract text from the PDF
text <- pdf_text(pdf_file)

# Convert the text into a tidy format
tidy_text <- tibble(line = text) %>%
  unnest_tokens(word, line) %>%
  anti_join(stop_words)

# Perform basic text analysis
word_counts <- tidy_text %>%
  count(word, sort = TRUE)

# Show the most frequent words
top_words <- head(word_counts, 10)
print(top_words)

# Plot word frequency bar chart
word_freq_plot <- word_counts %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "skyblue") +
  labs(x = "Word", y = "Frequency", title = "Most Frequent Words in Oct MPS") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ coord_flip()
print(word_freq_plot)

# Create word cloud
wordcloud <- wordcloud(words = word_counts$word, freq = word_counts$n, max.words = 100, random.order = FALSE)
print(wordcloud)

# Create a word cloud with color-coded frequencies
wordcloud_color <- word_counts %>%
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  labs(title = "Word Cloud with Color-Coded Frequencies")
print(wordcloud_color)

library(plotly)
# Convert word cloud to a plotly object for interactive visualization

wordcloud_plotly <- ggplotly(wordcloud_color)

print(wordcloud_plotly)
