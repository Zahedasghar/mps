library(pdftools)
library(stringi)
library(tidyverse)
library(janitor)
library(tidytext)


mps_jan <- pdf_text("https://www.sbp.org.pk/m_policy/2024/MPS-Jan-2024-Eng.pdf")

# 

mps_jan

mp_jan <- mps_jan |>  tibble(line = 1, text = mps_jan)

save(mp_jan,file="data/mp_jan.RData")
load("data/mp_jan.RData") ## Monetary Policy Document

mp_jan
# 
mp_jan |> unnest_tokens(word, text)

library(stopwords)
mp_jan |> unnest_tokens(word, text) |> anti_join(stop_words)
# 
# 
mp_jan |> unnest_tokens(word, text) |> anti_join(stop_words) |>
  count(word,sort = TRUE) -> jan_tbl

jan_tbl |> View()


mp_jan |> unnest_tokens(word, text) |> anti_join(stop_words) |>
  count(word,sort = TRUE) |>
  filter(n>4) |> 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill="steelblue") +
  labs(y = NULL)+theme_minimal()+ labs(y="" ,title ="Terms used in MPS janober 2023",
                                       caption="SBP, text_analysis @Zahedasghar")


mpjan_sw <- mp_jan |>  unnest_tokens(word, text) |> anti_join(stop_words) 




mpjan_sw %>%
  count(word) %>%
  filter(n > 4) %>%
  ggplot(aes(x = reorder(word, n),
             y = n)) +
  geom_col() +
  coord_flip()


# Create a dataframe with the word count
word_frequencies_jan <-
  mpjan_sw %>%
  count(word)

# Create a dataframe with the word count
word_frequencies_jan <-
  mpjan_sw %>%
  count(word)



# Plot word cloud
 wordcloud::wordcloud(words = word_frequencies_jan$word,
                     freq = word_frequencies_jan$n,
                     random.order = FALSE,
                     scale = c(2, 0.5),
                     min.freq = 1,
                     max.words = 100,
                     colors = c("#6FA8F5",
                                "#FF4D45",
                                "#FFC85E")
)


 
 
library(ragg)
 
ragg::agg_png("ragg_10X10.png", width = 10, height = 10, res = 300)
wrd_plt
dev.off()
camcorder::gg_record(
  dir = 'images',
  width = 12,
  height = 12 * 9 / 16,
  dpi = 300,
  bg = 'white' 
  # Makes sure background of plot is actually white, not transparent
)
## Save the word cloud

library(ragg)

ragg::agg_png("ragg_10X10.png", width = 10, height = 10, res = 300)
wrd_plt
dev.off()
