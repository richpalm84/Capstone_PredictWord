#Packages for this project.
library(quanteda)
#library(readtext)
#library(LaF)
#library(NLP)
#library(stringr)
library(dplyr)
library(tidyr)

#setwd("~/Richys Docs/Data Science Course/Capstone/Coursera-SwiftKey/final/en_US")
#fourGram <- readRDS("./fourGramFile.rds")
#triGram <- readRDS("./triGramFile.rds")
#biGram <- readRDS("./biGramFile.rds")

fourGram <- readRDS("data/fourGramFile.rds")
triGram <- readRDS("data/triGramFile.rds")
biGram <- readRDS("data/biGramFile.rds")

#fourGram <- filter(fourGram, Frequency > 1)
#triGram <- filter(triGram, Frequency > 1)
#biGram <- filter(biGram, Frequency > 1)

#this function works to tokenize the text input by the user
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

#This is the prediction function
fun.predict = function(inputWords) {

  #This uses the tokenize function (above) and makes it into a dataframe.  It also only takes the last three words of the input
  y = data_frame(word = fun.tokenize(inputWords))
  y <- tail(y, 3)

  #This uses the three word input starts with the fourgram and works down if a match isn't found. 
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
  #If only two words were entered then this starts with the trigram and works down.
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
  #If only one word was entered this starts with the bigram and then goes to the default word if not found.
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