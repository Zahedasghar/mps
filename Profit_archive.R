library(rvest)
library(dplyr)


library(tidyverse)
library(tidytext)
library(dplyr)
library(wordcloud)
library(tm)
library(SnowballC)

# Initialize an empty list to store story titles
all_story_titles <- list()

# Specify the base URL of the Tribune Pakistan website
base_url <- "https://profit.pakistantoday.com.pk/category/headlines/"

# Loop through each day for which you want to scrape data (February 23 and 24)
for (day in c(15, 24)) {
  # Construct the URL for the specific day
  url <- paste0(base_url, "?paged=", day)  # Adjust URL parameters based on website pagination
  
  # Read the HTML content of the webpage
  page <- read_html(url)
  
  # Scrape the story titles using a CSS selector (update if necessary)
  story_titles <- page %>%
    html_nodes("h3") %>%  # Update CSS selector here
    html_text()
  
  # Store the story titles for the current day in the list
  all_story_titles[[day]] <- story_titles
}

# Combine the story titles into a single data frame
all_story_titles_df <- bind_rows(lapply(seq_along(all_story_titles), function(day) {
  data.frame(
    Date = rep(day, length(all_story_titles[[day]])),
    Story_Title = all_story_titles[[day]],
    stringsAsFactors = FALSE
  )
}))

# Print the scraped story titles
print(all_story_titles_df)




# Keywords related to economy
economy_keywords <- c("economy", "economic", "GDP", "inflation", "unemployment", "fiscal policy", "monetary policy", 
                      "circular debt","prices","inflation", "trade", "exports", "imports", "tariffs", "budget", "deficit", "surplus", "investment", 
                      "stock market", "recession", "growth")


# Filter story titles containing economy-related keywords
econ_pr <- grep(paste(economy_keywords, collapse = "|"), all_story_titles_df, value = TRUE, ignore.case = TRUE)

# Print the economy-related titles
print(econ_pr)


economy_tibble_profit <- data.frame(headline = econ_pr) %>%
  unnest_tokens(word, headline)

economy_tibble_profit |> dim()
economy_tibble <- economy_tibble_profit %>%
  anti_join(stop_words)



# Perform sentiment analysis
sentiment_analysis <- inner_join(get_sentiments("bing"), economy_tibble, by = c("word" = "word"))

# Count word frequencies
word_freq <- count(sentiment_analysis, word)

# Create a word cloud
wordcloud(words = word_freq$word, freq = word_freq$n,
          min.freq = 1, max.words = 300, random.order = FALSE, 
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
  labs(x = "Sentiment Score", y = "Frequency", title = "Distribution of Sentiment Scores for last 5 days of Economy-related Headlines",subtitle = "The Profit AFINN Lexicon",
       caption = "By Zahid Source: The Profit")



