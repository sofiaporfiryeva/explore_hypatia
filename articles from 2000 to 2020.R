library(rjson)
library(dplyr)
library(tidyverse)
library(tm)
library(topicmodels)
library(ggplot2)
library(udpipe)
library(tokenizers)

json_data_2000_2020 <- json_data[sapply(json_data, function(x) {
  year <- as.numeric(substr(x$datePublished, 1, 4))
  year >= 2000 & year <= 2020
})]

# Making a dataframe

df_2000_2020 <- map_df(json_data_2000_2020, ~as.data.frame(t(.))) 

# Cleaning and filtering the data

df_2000_2020 <- df_2000_2020[df_2000_2020$pageCount >= 10, ] # filtered by page number

df_2000_2020$unigrams <- sapply(df_2000_2020$unigramCount,
                                function(x) paste(names(x), collapse = " "))
df_2000_2020$unigrams <- gsub("[[:punct:]0-9]+", "", df_2000_2020$unigrams)

df_2000_2020 <- df_2000_2020 %>%
  mutate(unigrams_cleaned = sapply(unigrams, function(x) removeWords(x, stopwords("english"))))

df_2000_2020 <- df_2000_2020 %>%
  distinct(doi, .keep_all = TRUE) # removing duplicates dois


# Tokenization of the unigrams

df_2000_2020$tokens <- sapply(df_2000_2020$unigrams_cleaned, function(x) {
  tokenize_words(x, lowercase = TRUE, strip_punct = TRUE)
})

df_tokens <- df_2000_2020 %>%
  select(doi, tokens) %>%
  group_by(doi) %>%
  unnest(tokens)

df_tokens <- df_tokens %>%
  filter(tokens != "" & nchar(tokens) > 1)

# Handaling NA in dois column

library(purrr)

df_tokens <- df_tokens %>%
  mutate(doi = map(doi, ~ if (is.null(.)) NA else .)) 

df_tokens <- df_tokens %>%
  mutate(doi = unlist(doi))

df_tokens <- df_tokens %>%
  filter(!is.na(doi))

df_text <- df_tokens %>%
  group_by(doi) %>%
  summarize(text = paste(tokens, collapse = " "))

#n_unique_doi <- n_distinct(df_tokens$doi)
#print(n_unique_doi) # checking the unique dois values
