# Install and load the required packages
# install.packages("pdftools")
# install.packages("tm")
# install.packages("RWeka")
# install.packages("ggplot2")
library(pdftools)
library(tm)
library(RWeka)
library(ggplot2)

# Extract text from the PDF document
pdf_text <- pdf_text("https://www.sbp.org.pk/m_policy/2023/MPS-Dec-2023-Eng.pdf")

# Extract text from the PDF
#pdf_text <- pdf_text("EconFest.pdf")


# Combine the text from all pages
pdf_text_combined <- paste(pdf_text, collapse = "\n")

# Create a Corpus
corpus <- Corpus(VectorSource(pdf_text_combined))

# Preprocess the text
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# Create bigrams using a custom function
create_bigrams <- function(text) {
  words <- unlist(strsplit(text, " "))
  bigrams <- paste(words[-length(words)], words[-1], sep = " ")
  return(bigrams)
}

corpus <- tm_map(corpus, content_transformer(create_bigrams))


# Extract the bigrams from the corpus
bigrams <- unlist(lapply(corpus, as.character))

# Create a data frame with bigram frequencies
bigram_df <- data.frame(bigram = bigrams)

# Count the frequency of each bigram
bigram_counts <- table(bigram_df$bigram)
bigram_df <- data.frame(bigram = names(bigram_counts), count = as.numeric(bigram_counts))

# Sort by frequency in descending order
bigram_df <- bigram_df[order(-bigram_df$count), ]

# Create a bar chart
 ggplot(bigram_df[1:15, ], aes(x = reorder(bigram, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Bigram", y = "Frequency", title="MPS summarised ",
       subtitle = "Monetary Policy Statement Dec 2023 ") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Load the wordcloud package
 library(wordcloud)
 
 # Create a word cloud from the bigram data
 wordcloud(words = bigram_df$bigram, freq = bigram_df$count, scale=c(1,0.2),
           min.freq = 2, colors = brewer.pal(10, "Dark2"))

 camcorder::gg_record(
   dir = 'images',
   width = 15,
   height = 12 * 9 / 16,
   dpi = 300,
   bg = 'white' 
   # Makes sure background of plot is actually white, not transparent
 )
 




# tri-gram ----------------------------------------------------------------

corpus <- Corpus(VectorSource(pdf_text))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

trigram_tokenizer <- function(x) {
  unlist(lapply(strsplit(x, " "), function(words) {
    n <- length(words)
    if (n < 3) return(character(0))
    trigrams <- paste0(words[1:(n-2)], words[2:(n-1)], words[3:n])
    return(trigrams)
  }))
}


corpus <- tm_map(corpus, content_transformer(trigram_tokenizer))

trigrams <- unlist(corpus)
trigram_df <- data.frame(trigram = trigrams)
trigram_counts <- table(trigram_df$trigram)
trigram_df <- data.frame(trigram = names(trigram_counts), count = as.numeric(trigram_counts))

trigram_df <- trigram_df[order(-trigram_df$count), ]

ggplot(trigram_df[1:10, ], aes(x = reorder(trigram, count), y = count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(x = "Trigram", y = "Frequency") +
  ggtitle("Top Trigrams in the Text") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


