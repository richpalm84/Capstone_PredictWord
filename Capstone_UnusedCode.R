#This first section is a copy of my final algorithm

library(quanteda)
library(readtext)
library(LaF)
library(NLP)
library(stringr)
library(dplyr)
library(tidyr)
setwd("~/Richys Docs/Data Science Course/Capstone/Coursera-SwiftKey/final/en_US")
#Get the data files
fourGram <- readRDS("./fourGramFile.rds")
triGram <- readRDS("./triGramFile.rds")
biGram <- readRDS("./biGramFile.rds")

#Removes occurencse of just one.
fourGram <- filter(fourGram, Frequency > 1)
triGram <- filter(triGram, Frequency > 1)
biGram <- filter(biGram, Frequency > 1)

#this function works to tokenize an input text
fun.tokenize = function(inputWords, ngramSize = 1, simplify = T) {
  tolower(
    quanteda::tokens(inputWords,
                     removeNumbers = T,
                     removePunct = T,
                     removeSeparators = T,
                     removeTwitter = T,
                     ngrams = ngramSize,
                     concatenator = " "
    )
  )
}
#this function tests for NA and is fatster than predict1
fun.predict = function(inputWords) {
  y = data_frame(word = fun.tokenize(inputWords))
  y <- tail(y, 3)
  if (nrow(y)==3) {
    y <- unlist(y)
    y <- t(y)
    y <- data.frame(y)
    y <- unite(y, col="cleanInput", sep = " ")
    test <- grep(y, fourGram$words)
    if (length(test)>0) {
      selectedRows <- fourGram[grep(y , fourGram$words), ]
      selectedRows <- arrange(selectedRows, desc(Frequency))
      selectedRows[1,2]
    }
    else if (length(test)==0) {
      test <- grep(y, triGram$words1.2)
      if (length(test)>0) {
        selectedRows <- triGram[grep(y , triGram$words1.2), ]
        selectedRows <- arrange(selectedRows, desc(Frequency))
        selectedRows[1,2]
      }
      else if (length(test)==0) {
        test <- grep(y, biGram$words)
        if (length(test)>0) {
          selectedRows <- biGram[grep(y , biGram$words), ]
          selectedRows <- arrange(selectedRows, desc(Frequency))
          selectedRows[1,2]
        }
        else print("the")
      }
    }
  }
  else if (nrow(y)==2) {
    y <- unlist(y)
    y <- t(y)
    y <- data.frame(y)
    y <- unite(y, col="cleanInput", sep = " ")
    test <- grep(y, triGram$words)
    if (length(test)>0) {
      selectedRows <- triGram[grep(y , triGram$words1.2), ]
      selectedRows <- arrange(selectedRows, desc(Frequency))
      selectedRows[1,2]
    }
    else if (length(test)==0) {
      test <- grep(y, biGram$words)
      if (length(test)>0) {
        selectedRows <- biGram[grep(y , biGram$words), ]
        selectedRows <- arrange(selectedRows, desc(Frequency))
        selectedRows[1,2]
      }
      else print("the")
    }
  }
  else if (nrow(y)==1) {
    y <- unlist(y)
    y <- t(y)
    y <- data.frame(y)
    y <- unite(y, col="cleanInput", sep = " ")
    test <- grep(y, biGram$words)
    if (length(test)>0) {
      selectedRows <- biGram[grep(y , biGram$words), ]
      selectedRows <- arrange(selectedRows, desc(Frequency))
      selectedRows[1,2]
    }
    else print("the")
  }
}



#All below is unused code for the capstone project.

#this function tests for na but doesn't move to n-1 grams and is slower.
fun.predict1 = function(inputWords) {
  y = data_frame(word = fun.tokenize(inputWords))
  y <- tail(y, 3)
  y <- unlist(y)
  y <- t(y)
  y <- data.frame(y)
  y <- unite(y, col="cleanInput", sep = " ")
  test4 <- grep(y, fourGram$words)
  test3 <- grep(y, triGram$words1.2)
  test2 <- grep(y, triGram$words)
  if (length(test4)>0) {
    selectedRows <- fourGram[grep(y , fourGram$words), ]
    selectedRows <- arrange(selectedRows, desc(Frequency))
    selectedRows[1,2]
  }
  else if (length(test3)>0) {
    selectedRows <- triGram[grep(y , triGram$words1.2), ]
    selectedRows <- arrange(selectedRows, desc(Frequency))
    selectedRows[1,2]
  }
  else if (length(test2)>0) {
    selectedRows <- fourGram[grep(y , fourGram$words), ]
    selectedRows <- arrange(selectedRows, desc(Frequency))
    selectedRows[1,2]
  }
  else print("the")
}




#first attempt.  Works but does not test for NA and move to n-1 grams.
fun.predict2 = function(inputWords) {
  
  if (length(inputWords)==1) {
    y = data_frame(word = fun.tokenize(inputWords))
  }
  
  if (nrow(y)==1) {
    y <- unlist(y)
    y <- t(y)
    y <- data.frame(y)
    y <- unite(y, col="cleanInput", sep = " ")
    selectedRows <- biGram[grep(y, biGram$words), ]
    selectedRows <- arrange(selectedRows, desc(Frequency))
    selectedRows[1,2]
  }   
  else if (nrow(y)==2) {
    y <- unlist(y)
    y <- t(y)
    y <- data.frame(y)
    y <- unite(y, col="cleanInput", sep = " ")
    selectedRows <- triGram[grep(y , triGram$words1.2), ]
    selectedRows <- arrange(selectedRows, desc(Frequency))
    selectedRows[1,2]
  }
  else if (nrow(y)==3) {
    y <- unlist(y)
    y <- t(y)
    y <- data.frame(y)
    y <- unite(y, col="cleanInput", sep = " ")
    selectedRows <- fourGram[grep(y , fourGram$words), ]
    selectedRows <- arrange(selectedRows, desc(Frequency))
    selectedRows[1,2]
  }
  else if (nrow(y)==4) {
    inputWords <- word(inputWords, 2, 4)
    y <- unlist(y)
    y <- t(y)
    y <- data.frame(y)
    y <- unite(y, col="cleanInput", sep = " ")
    selectedRows <- fourGram[grep(inputWords , fourGram$words), ]
    selectedRows <- arrange(selectedRows, desc(Frequency))
    selectedRows[1,2]
  }  
  else print("Sorry! Try a different phrase")
}


#Creates a wordcloud from dfm object
#topfeatures(dfm_all, 20) ## See words that appear the most

#set.seed(100)
#textplot_wordcloud(dfm_all, min_count = 5, random_order = FALSE,
#                  rotation = .25, 
#                 color = RColorBrewer::brewer.pal(8,"Dark2"))

#summary(corpus_tw_sample, 5)

#Unused trial code:

#toks_all <- tokens_remove(toks_all, min_nchar=2, max_nchar=79)
#toks_all <- tokens(toks_all, ngrams=1:5)

##"â"

##Reading in the full twitter data set
##twitter_all <- readtext("en_US.twitter.txt", cache = FALSE)
##corpus_twitter_all <- corpus(twitter_all)
##summary(corpus_twitter_all)

##Reading in a certain number of files from twitter package using tm package
##library(tm)
##con <- file("en_US.twitter.txt", "r")
##twitter_small <- readLines(con, 1:5000) 
##close(con) 
##corpus_tw_sm <- corpus(twitter_small)

#This splits my bigram string into separate columns
#bigram2 <- do.call(rbind, strsplit(trigrams, ' '))
#This binds it back to the bigram_count dataframe so I can look at separate words
#bigram3 <- cbind(bigram2, bigram_count)

#THIS WORKS TO CREATE A DATAFRAME WITH MY PHRASE, WITHOUT SPACES.  DOESN'T HAVE FREQUENCY COLUMN
#df4 <- data.frame(Content = df3, stringsAsFactors = F)

#splt_words <- strsplit(df$Content, "_", fixed = T)

#another way to produce a dataframe with frequencies:
#df2 <- textstat_frequency(dfm_all, n = NULL, groups = NULL)

# Remove the "_" between the words after creating a dataframe from the dfm object.
#df3 <- str_replace_all(df$Content, "_", " ")

#To find frequency of certain text
#df[df$Content=="how_are_you",]
#df2[df2$feature=="how_are",]

#This will spit out a the first two words of each of my trigrams in my df and put provide a frequency for each.
#df %>% 
#  dplyr::mutate(first_2_words = stringr::word(df$Content, 1, 2)) %>% 
#  count(first_2_words, sort = TRUE) 

#This will search for the string and return all the rows of content that match the input string
#df %>%
#  filter(str_detect(Content, "went to"))

#Method to make go from corpus to frequency count and create a data frame with trigram counts
#removed [1] from first line and [[1]] from second line but by doing this the result messed up. 
#corpus_all_str = as.character(corpus_all)
#splt_words = strsplit(corpus_all_str, " ", fixed = T)
#trigrams = vapply(ngrams(splt_words, 3), paste, "", collapse = " ")
#trigram_count = as.data.frame(xtabs(~trigrams))
#head(trigram_count[order(trigram_count$Freq, decreasing = T),])
