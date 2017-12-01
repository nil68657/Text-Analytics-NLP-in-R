# Text analysis using tidytext and dplyr in R using Jeopardy Dataset

# install tidytext and dplyr packages
install.packages("tidytext", repos = "https://cran.r-project.org")
install.packages("dplyr", repos = "https://cran.r-project.org")
install.packages("ggplot2", repos = "https://cran.r-project.org")
install.packages("tm", repos="https://cran.r-project.org")
install.packages("SnowballC", repos="https://cran.r-project.org")
install.packages("wordcloud", repos="https://cran.r-project.org")
install.packages("RColorBrewer", repos="https://cran.r-project.org")


# load libraries
library(dplyr)
library(tidytext)
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# change the working directory using setwd() function to the 
# folder where you have copied the data file
setwd("C:/Users/nilan/Desktop/Big Data/Text_Analytics")

# read the Jeopardy dataset
JeopardyData <- read.csv('JEOPARDY_CSV.csv', stringsAsFactors = FALSE)

# extract only the IncidentDescription column into a dataset
QuestionData <- JeopardyData$Question

# convert the data to a data frame
text_df <- data_frame(line = 2:216931, text = QuestionData)
head(text_df)

# tokenize with standard tokenization using unnext_tokens from tidytext
token_data <- unnest_tokens(text_df, word, text)

# remove stop-words using anti_join function from dplyr
# stop_words come from tidytext package
token_data <- anti_join(token_data, stop_words)

# use the count() function of dplyr to view most common words
wordcount <- count(token_data,word, sort = TRUE)

# filter for n>5000 using filter function from dplyr
wordcountfiltered <- filter(wordcount, n > 2000)

# visualize with ggplot
ggplot(wordcountfiltered, aes(reorder(word, n), n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

incidentCorpus <- Corpus(VectorSource(claimsData$IncidentDescription))

incidentCorpus <- tm_map(incidentCorpus, content_transformer(tolower))

incidentCorpus <- tm_map(incidentCorpus, removePunctuation)
incidentCorpus <- tm_map(incidentCorpus, PlainTextDocument)
incidentCorpus <- tm_map(incidentCorpus, removeWords, stopwords('english'))
incidentCorpus <- tm_map(incidentCorpus, stemDocument)

incidentCorpus <- Corpus(VectorSource(incidentCorpus))

wordcloud(incidentCorpus, max.words = 10000, random.order = FALSE, colors = brewer.pal(6, "Dark2"))


# the steps from count to ggplot can be combined using the piping operator %>%
# you can later lookup what piping means in R
token_data %>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()
