library(ggplot2)

ggplot(prevalence_df, aes(x = as.factor(Topic), y = Prevalence)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Topic Prevalence", x = "Topic", y = "Prevalence") +
  theme_minimal() +
  coord_flip() 
# extracting the dois

gamma_matrix <- as.data.frame(lda_model@gamma)
gamma_matrix$doi <- df_text$doi 

threshold <- 0.2  # Example threshold, adjust as needed
topic_13_docs <- gamma_matrix %>%
  filter(V13 > threshold) %>%  
  select(doi)

df_2000_2020 <- df_2000_2020 %>%
  mutate(doi = sapply(doi, function(x) ifelse(is.null(x), NA, as.character(x))))

topic_13_docs <- topic_13_docs %>%
  left_join(df_2000_2020 %>%
              select(doi, creator, publicationYear, issueNumber), 
            by = "doi")

topic_13_docs <- topic_13_docs %>%
  mutate(publicationYear = as.numeric(publicationYear))

most_frequent_years <- topic_13_docs %>%
  group_by(publicationYear) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))


# graph

ggplot(most_frequent_years, aes(x = publicationYear, y = frequency)) +
  geom_line(color = "lightblue") +   # Line plot
  geom_point(color = "blue") +   # Add points for clarity
  labs(title = "Frequency of Topic 13: Feminist Ethics of Care",
       x = "Publication Year",
       y = "Number of Articles") +
  scale_x_continuous(breaks = seq(min(most_frequent_years$publicationYear), 
                                  max(most_frequent_years$publicationYear), by = 2)) +  
  scale_y_continuous(limits = c(0, max(most_frequent_years$frequency) * 1.1)) + 
  theme_minimal()
