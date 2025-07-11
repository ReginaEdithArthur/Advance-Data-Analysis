#Set working directory
setwd("C:/Users/LENOVO/Desktop/Advance Data Analysis Exam/Covid_19_Topic_Modelling_Project")

# Install required packages (if not already installed)
install.packages(c("tidyverse", "tm", "textstem", "tidytext", "wordcloud", "ggplot2"))

# Load libraries
library(tidyverse)
library(tm)
library(textstem)
library(tidytext)
library(wordcloud)
library(ggplot2)

# Load dataset
tweets <- read_csv("covid19_tweets.csv")
tweets <- tweets %>% slice(1:2000)

# Check basic structure
glimpse(tweets)

# Preprocessing: Clean text
clean_text <- tweets %>%
  select(text) %>%
  mutate(text = tolower(text),
         text = str_replace_all(text, "http\\S+|https\\S+", ""),  # Remove URLs
         text = str_replace_all(text, "[^[:alpha:]\\s]", ""),     # Remove punctuation/numbers
         text = removeWords(text, stopwords("en")),               # Remove stopwords
         text = stripWhitespace(text))                            # Remove extra whitespace

# Tokenization
tokens <- clean_text %>%
  unnest_tokens(word, text)

# Lemmatization
tokens <- tokens %>%
  mutate(word = lemmatize_words(word))

# Remove empty words and low-information tokens
tokens <- tokens %>%
  filter(nchar(word) > 2)

# Word frequency
word_freq <- tokens %>%
  count(word, sort = TRUE)

# View top 20 words
print(head(word_freq, 20))

# Word cloud 
set.seed(123)
wordcloud(words = word_freq$word,
          freq = word_freq$n,
          min.freq = 20,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))

# LDA using textmineR for topic tuning

# Install alternative package
install.packages("textmineR")

# Load package
library(textmineR)

# Reconstruct cleaned documents
clean_docs <- tokens %>%
  group_by(doc_id = row_number()) %>%
  summarise(text = paste(word, collapse = " "))

# Create a document vector for textmineR
doc_vec <- clean_docs$text
names(doc_vec) <- paste0("doc", seq_along(doc_vec))  # Assign document names

# Create a DTM using textmineR's own function
dtm <- CreateDtm(doc_vec,
                 doc_names = names(doc_vec),
                 ngram_window = c(1, 1),   # Unigrams
                 stopword_vec = stopwords::stopwords("en"),
                 lower = TRUE,
                 remove_punctuation = TRUE,
                 remove_numbers = TRUE,
                 verbose = FALSE)

# Fit models for a range of topic numbers
k_list <- seq(2, 8, by = 2)
models <- lapply(k_list, function(k) {
  FitLdaModel(dtm = dtm, k = k, iterations = 200, burnin = 80,
              alpha = 0.1, beta = 0.05, optimize_alpha = TRUE,
              calc_likelihood = TRUE, calc_coherence = TRUE, calc_r2 = TRUE)
})

# Extract coherence scores
coherence_scores <- sapply(models, function(m) mean(m$coherence))

# Plot to select best number of topics
plot(k_list, coherence_scores, type = "b", pch = 19,
     xlab = "Number of Topics", ylab = "Mean Coherence Score",
     main = "Topic Coherence vs. Number of Topics")

# Print topic numbers with their corresponding coherence scores
coherence_results <- data.frame(
  Topics = k_list,
  Coherence = coherence_scores
)

print(coherence_results)


# Set optimal number of topics (this number based is on the coherence plot)
optimal_k <- 6  

# Fit final model
final_model <- FitLdaModel(dtm = dtm, 
                           k = optimal_k, 
                           iterations = 200, burnin = 80,
                           alpha = 0.1, beta = 0.05,
                           optimize_alpha = TRUE,
                           calc_likelihood = TRUE, 
                           calc_coherence = TRUE,
                           calc_r2 = TRUE)

# Top terms per topic
top_terms <- GetTopTerms(final_model$phi, M = 10)
print(top_terms)

# Document-topic distributions
theta <- final_model$theta
head(theta)

# Assign dominant topic per document
doc_topics <- data.frame(doc_id = rownames(theta),
                         dominant_topic = apply(theta, 1, which.max))
head(doc_topics)

# Show example documents per topic
for (t in 1:optimal_k) {
  cat(paste0("\n=== Topic ", t, " ===\n"))
  sample_docs <- clean_docs$text[doc_topics$dominant_topic == t][1:2]  # Show 2 samples
  print(sample_docs)
}


# Load LDAvis if not already installed
install.packages("LDAvis")
install.packages("slam")

# Ensure LDAvis and slam are loaded
library(Matrix)
library(slam)

# Convert original dtm (used in FitLdaModel) to sparse matrix
dtm_used <- as(dtm, "dgCMatrix")

# Calculate document lengths and term frequencies
doc_length <- slam::row_sums(dtm_used)
term_frequency <- slam::col_sums(dtm_used)

# Create LDAvis JSON
json_lda <- createJSON(
  phi = final_model$phi,
  theta = final_model$theta,
  vocab = final_model$vocab,
  doc.length = doc_length,
  term.frequency = term_frequency
)

# Visualize
serVis(json_lda)




