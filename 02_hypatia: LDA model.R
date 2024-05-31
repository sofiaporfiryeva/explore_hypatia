library(tidyverse)
library(tm)
library(topicmodels)
library(tidyverse)
library(tm)
library(topicmodels)
library(ggplot2)
library(udpipe)

# This is an LDA model for article from 2000 to 2020

# unique years
years <- sapply(json_data, function(x) {
  as.numeric(substr(x$datePublished, 1, 4))
})


unique_years <- unique(years)

print(unique_years)

# Extracting articles from 2000 to 2020
json_data_2000_2020 <- json_data[sapply(json_data, function(x) {
  year <- as.numeric(substr(x$datePublished, 1, 4))
  year >= 2000 & year <= 2020
})]


df_2000_2020 <- map_df(json_data_2000_2020, ~as.data.frame(t(.)))
df_2000_2020 <- df_2000_2020[df_2000_2020$pageCount >= 10, ]

# Cleaning
df_2000_2020$unigrams <- sapply(df_2000_2020$unigramCount,
                                function(x) paste(names(x), collapse = " "))
df_2000_2020$unigrams <- gsub("[[:punct:]0-9]+", "", df_2000_2020$unigrams)

# Making corpus
corpus <- Corpus(VectorSource(df_2000_2020$unigrams))

# Stop words
stop_words <- stopwords("en")
additional_stop_words <- c("one")
all_stop_words <- c(stop_words, additional_stop_words)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, all_stop_words)
corpus <- tm_map(corpus, stripWhitespace)

# Making matrix
dtm <- DocumentTermMatrix(corpus)
dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ] 

# Building LDA
lda_model <- LDA(dtm, k = 10) # k - количество тем


# Extracting key terms
top_words <- lda_model@beta %>% 
  as.data.frame() %>% 
  mutate(topic = 1:nrow(.)) %>% 
  gather(word, weight, -topic) %>% 
  group_by(topic) %>%
  top_n(10, wt = weight) %>% 
  mutate(x = 1:n(), 
         y = weight) 

word_mapping <- setNames(colnames(dtm), paste0("V", 1:ncol(dtm)))
top_words <- top_words %>%
  mutate(word = word_mapping[word])

# Lemmatization 
udpipe_download_model(language = "english")
model <- udpipe_load_model("english")

lem <- udpipe_annotate(model, x = top_words$word)
lem <- as.data.frame(lem)
top_words$word <- lem$lemma[match(top_words$word, lem$token)]

# Building key terms without 'woman' 
stop_words <- stopwords("en")
additional_stop_words <- c("one", "woman")  # Добавляем слово "one" к списку стоп-слов
all_stop_words <- c(stop_words, additional_stop_words)
top_words <- top_words %>% 
  filter(!word %in% all_stop_words)

# Building plot
ggplot(top_words, aes(x = reorder(word, y), y = y, fill = topic)) + 
  geom_col() +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() + 
  labs(x = "Term", y = "Topics", title = "Key terms") +
  theme_bw()
