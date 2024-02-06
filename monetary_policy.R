library(pdftools)
library(stringi)
library(tidyverse)
library(janitor)
library(tidytext)


mps_sep <- pdf_text("https://www.sbp.org.pk/m_policy/2023/MPS-Sep-2023-Eng.pdf")

# 

mps_sep

mp_sep <- mps_sep |>  tibble(line = 1, text = mps_sep)

save(mp_sep,file="data/mp_sep.RData")
load("data/mp_sep.RData") ## Monetary Policy Document

mp_sep
# 
mp_sep |> unnest_tokens(word, text)

library(stopwords)
mp_sep |> unnest_tokens(word, text) |> anti_join(stop_words)
# 
# 
mp_sep |> unnest_tokens(word, text) |> anti_join(stop_words) |>
  count(word,sort = TRUE) -> sep_tbl

jul_tbl |> View()

sep_tbl |> View()

mp_sep |> unnest_tokens(word, text) |> anti_join(stop_words) |>
  count(word,sort = TRUE) |>
  filter(n>4) |> 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill="steelblue") +
  labs(y = NULL)+theme_minimal()+ labs(y="" ,title ="Terms used in MPS Sep 2023",
                                       caption="SBP, text_analysis @Zahedasghar")


mpsep_sw <- mp_sep |>  unnest_tokens(word, text) |> anti_join(stop_words) 




mpsep_sw %>%
  count(word) %>%
  filter(n > 4) %>%
  ggplot(aes(x = reorder(word, n),
             y = n)) +
  geom_col() +
  coord_flip()


# Create a dataframe with the word count
word_frequencies_sep <-
  mpsep_sw %>%
  count(word)




# Plot word cloud
wordcloud::wordcloud(words = word_frequencies_sep$word,
                     freq = word_frequencies_sep$n,
                     random.order = FALSE,
                     scale = c(2, 0.5),
                     min.freq = 1,
                     max.words = 100,
                     colors = c("#6FA8F5",
                                "#FF4D45",
                                "#FFC85E")
)

