
# Working with the first chunk only; first, saving the first chunk:

first_chunk <- chunks[[1]]

# Building data frame with the first chunk;

library(dplyr)
library(purrr)


first_chunk_df <- map_df(first_chunk, ~as.data.frame(t(.)))

# 'Creator' column: multiple creators > single string; empty lists > NA

first_chunk_df$creator <- sapply(first_chunk_df$creator, function(x) {
  if(length(x) > 0) {
    return(paste(x, collapse = "; "))
  } else {
    return(NA)
  }
})

# Unigrams from list to single string;
    
first_chunk_df <- first_chunk_df %>%
      mutate(unigramCount = sapply(unigramCount, function(x) {
        if(length(x) > 0) {
          return(paste(names(x), collapse = "; "))
        } else {
          return("")
        }
      }))

# Cleaning time!

library(tidyverse)


clean_unigramCount <- function(text) {
  text <- tolower(text)
  text <- gsub("[0-9]", "", text)
  text <- gsub("[[:punct:]]", "", text)
  text <- removeWords(text, stopwords("en"))
  text <- gsub("\\b\\w{1,3}\\b", "", text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  return(text)
}

first_chunk_df <- first_chunk_df %>%
  mutate(unigramCount = sapply(unigramCount, clean_unigramCount))

first_chunk_df <- as.data.frame(first_chunk_df)


# Creating document-term matrix:

install.packages("topicmodels")
library(topicmodels)
library(tidytext)
library(vctrs)
library(tm)
library(tokenizers)

# Building TermDocumentMatrix

# Tokenizing each document

tokenize_text <- function(text) {
  tokens <- tokenize_words(text, strip_punct = TRUE, lowercase = TRUE)
  return(tokens[[1]])  # Extract the tokens from the list
}

documents <- lapply(first_chunk_df$unigramCount, tokenize_text)

# Building a corpus from the tokenized document: 

myCorpus <- Corpus(VectorSource(documents))
tdm <- TermDocumentMatrix(myCorpus)

####

library(wordcloud2)

# Convert tdm to a regular matrix
tdm_matrix <- as.matrix(tdm)

# Calculate term frequencies
term_freqs <- rowSums(tdm_matrix)

wordcloud_data <- data.frame(term = names(term_freqs), freq = term_freqs) %>%
  mutate(term = gsub('"', '', term), 
         term = gsub(",", '', term)) %>%
  filter(term != "hypatia") %>%
  arrange(desc(freq)) %>% 
  slice(1:80)

# Generate word cloud
wordcloud2(data = wordcloud_data, size = 0.7, color = "random-dark")

####

# Silge, Julia, and David Robinson. Text Mining with R: A Tidy Approach, 2017
# p. 87

ap_lda_model <- LDA(tdm, k = 2, control = list(seed = 1234))
ap_topics <- tidy(ap_lda_model, matrix = "beta")


# Defining most common terms using ggplot2 for visualization

library(ggplot2)

top_terms <- ap_topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>%
  ungroup() %>% 
  arrange(topic, -beta)

top_terms %>% 
  mutate(term = reorder(term,beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


# Problem with ggplot2; the x-axis (term) displays numbers instead of terms; need to investigate and fix this bug;
#########
