# tags for topics
top_terms <- terms(lda_model, 10)  # 10 terms per topic
top_terms_df <- as.data.frame(top_terms)
colnames(top_terms_df) <- paste0("Term", 1:ncol(top_terms_df))
  

# counting prevalence

topic_prevalence <- as.data.frame(posterior(lda_model)$topics)
topic_prevalence_summary <- colMeans(topic_prevalence)

prevalence_df <- data.frame(
  Topic = seq_len(length(topic_prevalence_summary)),
  Prevalence = topic_prevalence_summary
)
