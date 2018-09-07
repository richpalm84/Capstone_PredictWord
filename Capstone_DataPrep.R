#Packages used
library(quanteda)
library(readtext)
library(LaF)
library(NLP)
library(stringr)
library(dplyr)
library(tidyr)
setwd("~/Richys Docs/Data Science Course/Capstone/Coursera-SwiftKey/final/en_US")

## Reading in X number of random lines, creating the corpus, and basic dfm
set.seed(1234)
twitter_sample <- sample_lines("en_US.twitter.txt", 200000, nlines = 2360148)
blog_sample <- sample_lines("en_US.blogs.txt", 200000, nlines = 899288)
news_sample <- sample_lines("en_US.news.txt", 200000, nlines = 1010242)
corpus_tw_sample <- corpus(twitter_sample)
corpus_bl_sample <- corpus(blog_sample)
corpus_ne_sample <- corpus(news_sample)
rm(news_sample, blog_sample, twitter_sample)
corpus_all <- corpus_tw_sample + corpus_bl_sample + corpus_ne_sample
rm(corpus_bl_sample, corpus_ne_sample, corpus_tw_sample)

#Creating fourgram DF
toks_fourGram <- tokens(corpus_all, what = "fasterword", remove_numbers = T, remove_punct = T,
                        remove_symbols = T, remove_separators = TRUE,
                        remove_twitter = T, remove_hyphens = T, remove_url = T,
                        ngrams = 4, concatenator = " ",
                        verbose = quanteda_options("verbose"), include_docvars = F)

dfm_fourGram <- dfm(toks_fourGram, tolower=T, remove = stopwords("english"), stem = FALSE)
rm(toks_fourGram)

# This produces the fourgram dataframe with a frequency column
fourGram <- data.frame(Content = featnames(dfm_fourGram), Frequency = colSums(dfm_fourGram), 
                       row.names = NULL, stringsAsFactors = FALSE)
rm(dfm_fourGram)

#This code splits my ngrams into a single word per column
fourGram <- separate(fourGram, col="Content", sep=" ", into=c("word1", "word2", "word3", "predicted"))

#This puts the first two words into one column and leaves the word to predict in the other
fourGram <- unite(fourGram, "words", c("word1", "word2", "word3"), sep = " ")

#Output file to hardrive to read in later
saveRDS(fourGram, file = "./fourGramFile.rds")
rm(fourGram)


#Creating Trigram DF
toks_triGram <- tokens(corpus_all, what = "fasterword", remove_numbers = T, remove_punct = T,
                       remove_symbols = T, remove_separators = TRUE,
                       remove_twitter = T, remove_hyphens = T, remove_url = T,
                       ngrams = 3, concatenator = " ",
                       verbose = quanteda_options("verbose"), include_docvars = F)

#toks_triGram <- tokens_tolower(toks_triGram)


dfm_triGram <- dfm(toks_triGram, tolower=T, remove = stopwords("english"), stem = FALSE)
rm(toks_triGram)

# This produces the trigram dataframe with a frequency column
triGram <- data.frame(Content = featnames(dfm_triGram), Frequency = colSums(dfm_triGram), 
                      row.names = NULL, stringsAsFactors = FALSE)
rm(dfm_triGram)

#This code splits my ngrams into a single word per column

triGram <- separate(triGram, col="Content", sep=" ", into=c("word1", "word2", "predicted"))
#This puts the first two words into one column and leaves the word to predict in the other

triGram <- unite(triGram, "words1.2", c("word1", "word2"), sep = " ")

saveRDS(triGram, file = "./triGramFile.rds")
rm(triGram)


#Creating BiGram DF
toks_biGram <- tokens(corpus_all, what = "fasterword", remove_numbers = T, remove_punct = T,
                      remove_symbols = T, remove_separators = TRUE,
                      remove_twitter = T, remove_hyphens = T, remove_url = T,
                      ngrams = 2, concatenator = " ",
                      verbose = quanteda_options("verbose"), include_docvars = F)

#toks_biGram <- tokens_tolower(toks_biGram)

dfm_biGram <- dfm(toks_biGram, tolower=T, remove = stopwords("english"), stem = FALSE)
rm(toks_biGram)

# This produces the trigram dataframe with a frequency column
biGram <- data.frame(Content = featnames(dfm_biGram), Frequency = colSums(dfm_biGram), 
                     row.names = NULL, stringsAsFactors = FALSE)
rm(dfm_biGram)

#This code splits my ngrams into a single word per column
biGram <- separate(biGram, col="Content", sep=" ", into=c("word1", "predicted"))

#This puts the first two words into one column and leaves the word to predict in the other
biGram <- unite(biGram, "words", c("word1"), sep = " ")

saveRDS(biGram, file = "./biGramFile.rds")
rm(biGram)

rm(corpus_all)
