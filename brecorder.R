library(rvest)

# Specify the URL of the Tribune Pakistan website

# Function to scrape headlines from a given URL and date
scrape_headlines <- function(url, date, css_selector) {
  # Construct the URL for the specified date
  archive_url <- paste0(url, "archive/", date)
  
  # Read the HTML content of the webpage
  page <- read_html(archive_url)
  
  # Extract story titles using CSS selectors
  story_titles <- page %>%
    html_nodes(css_selector) %>%
    html_text()
  
  return(story_titles)
}

# URLs and CSS selectors for each website
urls <- c(
 # "https://www.dawn.com/",           # Dawn
 # "https://www.thenews.com.pk/",     # The News
  "https://www.brecorder.com/"       # Business Recorder
)

css_selectors <- c(
 # ".story__title",  # Dawn
 # "h2",             # The News
  "h2"              # brecorder
)

# Range of dates to scrape (in YYYY-MM-DD format)
start_date <- "2024-03-01"
end_date <- "2024-03-04"  # Today's date

dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
dates <- format(dates, "%Y-%m-%d")

# Scrape headlines from each website for each date
all_headlines <- lapply(seq_along(urls), function(i) {
  lapply(dates, function(date) {
    scrape_headlines(urls[i], date, css_selectors[i])
  })
})

# Flatten the list structure
all_headlines <- unlist(all_headlines, recursive = FALSE)

# Remove stop word "gold"
all_headlines <- gsub("\\bgold\\b", "", all_headlines)

# Display the updated headlines
head(all_headlines)

# Keywords related to economy
economy_keywords <- c("economy", "economic", "GDP", "inflation", "unemployment", "fiscal policy", "monetary policy", 
                     "IMF", "circular debt","prices","inflation", "trade", "exports", "imports", "tariffs", "budget", "deficit", "surplus", "investment", 
                      "stock market", "recession", "growth")


# Filter story titles containing economy-related keywords
econ_br <- grep(paste(economy_keywords, collapse = "|"), all_headlines, value = TRUE, ignore.case = TRUE)

# Print the economy-related titles
print(econ_br)


economy_tibble_brecorder <- data.frame(headline = econ_br) %>%
  unnest_tokens(word, headline)

economy_tibble_brecorder |> dim()
economy_tibble <- economy_tibble_brecorder %>%
  anti_join(stop_words)




library(tidyverse)
library(tidytext)
library(dplyr)
library(wordcloud)
library(tm)
library(SnowballC)

# Perform sentiment analysis
sentiment_analysis <- inner_join(get_sentiments("bing"), economy_tibble, by = c("word" = "word"))

# Count word frequencies
word_freq <- count(sentiment_analysis, word)

# Create a word cloud
wordcloud(words = word_freq$word, freq = word_freq$n,
          min.freq = 1, max.words = 200, random.order = FALSE, 
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
  geom_histogram(fill = "skyblue", bins = 20) +
  labs(x = "Sentiment Score", y = "Frequency", title = "Distribution of Sentiment Scores")



