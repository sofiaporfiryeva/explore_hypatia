# Building LDA model 

install.packages("ldatuning")
library(ldatuning) # a package that estimates the best number of topics

corpus <- Corpus(VectorSource(df_text$text))

# cleaning tems

trash_terms <- c("women", "woman", "one", "see", "sks")

dtm <- DocumentTermMatrix(corpus, control = list(
  wordLengths = c(3, Inf),   # Keep words with 3 or more characters
  removePunctuation = TRUE,
  stopwords = c(stopwords("english"), trash_terms),  # Include your custom stopwords
  removeNumbers = TRUE
))

# inspect(dtm[1:5, 1:5])

# Building LDA model

lda_model <- LDA(dtm, k = 30, control = list(seed = 1234))
top_terms <- terms (lda_model, 10)
print(top_terms)

