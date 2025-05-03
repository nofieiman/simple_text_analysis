#################################
# Step 1: Load and Clean the Data
#################################

# Install packages
install.packages("tidyverse")
install.packages("tidytext")
install.packages("tm")
install.packages("dplyr")

# Load necessary libraries
library(tidyverse)
library(tidytext)
library(tm)
library(dplyr)

# Load the text file
reviews <- readLines("/path/to/reviews.txt")

# Convert to a dataframe
reviews_df <- data.frame(text = reviews, stringsAsFactors = FALSE)

# Preprocess the text: remove punctuation, convert to lowercase, etc.
reviews_df$text <- tolower(reviews_df$text)
reviews_df$text <- removePunctuation(reviews_df$text)
reviews_df$text <- removeNumbers(reviews_df$text)
reviews_df$text <- removeWords(reviews_df$text, stopwords("en"))

# Tokenize the text into individual words
reviews_tokens <- reviews_df %>%
  unnest_tokens(word, text)


#####################################
# Step 2: Word Cloud for Common Words
#####################################

# Load the library
library(wordcloud)

# Generate a word cloud of the most frequent words
word_freq <- reviews_tokens %>%
  count(word, sort = TRUE)

wordcloud(words = word_freq$word, freq = word_freq$n, min.freq = 2,
          max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))


##################################################
# Step 3: Positive vs. Negative Word Contributions
##################################################

# Get positive and negative words
positive_words <- reviews_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

# Plot positive and negative word frequencies
positive_words %>%
  filter(n > 1) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Positive vs Negative Word Contributions",
       y = "Contribution to Sentiment", x = NULL) +
  coord_flip() +
  theme_minimal()


#################################################
# Step 4: Calculate the overall sentiment scores.
#################################################

# Get sentiment scores using the Bing lexicon
bing_sentiments <- reviews_tokens %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(total_words = positive + negative,
         sentiment_score = positive - negative)

# Display the scores
print(bing_sentiments)


#######################################
# Step 5 (optional): Visualise the plot
#######################################

# Load the library
library(ggplot2)

# Restructure data for ggplot
bing_long <- bing_sentiments %>%
  pivot_longer(cols = c("positive", "negative"), names_to = "sentiment", values_to = "count")

# Bar plot for positive vs negative sentiment counts
ggplot(bing_long, aes(x = sentiment, y = count, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Positive vs Negative Sentiment Counts",
       y = "Count of Words", x = NULL) +
  theme_minimal()
