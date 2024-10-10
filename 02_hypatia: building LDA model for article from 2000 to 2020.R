# Building LDA model 

library(tm)
library(topicmodels)

corpus <- Corpus(VectorSource(df_text$text))

# cleaning tems

trash_terms <- c("women", "woman", "one", "see", "sks")

dtm <- DocumentTermMatrix(corpus, control = list(
  wordLengths = c(3, Inf),  
  removePunctuation = TRUE,
  stopwords = c(stopwords("english"), trash_terms), 
  removeNumbers = TRUE
))


# Building LDA model

lda_model <- LDA(dtm, k = 30, control = list(seed = 1234))
top_terms <- terms (lda_model, 10)
print(top_terms)

