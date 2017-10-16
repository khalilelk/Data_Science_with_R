library(dplyr)
library(stringr)
library(shiny)

##Set workspace and load files
##-------------------------------------
##setwd("D:/RWSPACE/ShinyApps/NextWordPredict")

##Bigrams
bigram <- read.table("w2.txt",header = FALSE,fill = TRUE)
names(bigram) <- c("freq","word1","word2")
n2gram <- transform(bigram,word1=as.character(word1),word2=as.character(word2))
n2_gram <- n2gram[complete.cases(n2gram),]
n2_gram <- n2_gram[with(n2_gram,order(-freq)),]
table2 <- mutate(n2_gram,phrase=word1)

##Trigrams
trigram <- read.table("w3.txt",header = TRUE,fill = TRUE)
names(trigram) <- c("freq","word1","word2","word3")
n3gram <- transform(trigram,word1=as.character(word1),word2=as.character(word2),
                    word3=as.character(word3))
n3_gram <- n3gram[complete.cases(n3gram),]
n3_gram <- n3_gram[with(n3_gram,order(-freq)),]
table3 <- mutate(n3_gram,phrase=paste(word1,word2,sep = " "))

##quadgrams
quadgram <- read.table("w4.txt",header = TRUE,fill = TRUE)
names(quadgram) <- c("freq","word1","word2","word3","word4")
n4gram <- transform(quadgram,word1=as.character(word1),
                    word2=as.character(word2),word3=as.character(word3),
                    word4=as.character(word4))
n4_gram <- n4gram[complete.cases(n4gram),]
n4_gram <- n4_gram[with(n4_gram,order(-freq)),]
table4 <- mutate(n4_gram,phrase=paste(word1,word2,word3,sep = " "))

##pentagrams
pentagram <- read.table("w5.txt",header = TRUE,fill = TRUE)
names(pentagram) <- c("freq","word1","word2","word3","word4","word5")
n5gram <- transform(pentagram,word1=as.character(word1),
                    word2=as.character(word2),word3=as.character(word3),
                    word4=as.character(word4),word5=as.character(word5))
n5_gram <- n5gram[complete.cases(n5gram),]
n5_gram <- n5_gram[with(n5_gram,order(-freq)),]
table5 <- mutate(n5_gram,phrase=paste(word1,word2,word3,word4,sep = " "))


##Functions of prediction
##---------------------------------

##Predict second word using bigrams
second <- function(input){
      temp <- filter(table2,phrase==input)
      if (is.na(temp[1,3])) return("Cannot predict, please keep typing and try again")
      else return(head(temp[,3],n=1))
}

##Predict third word using trigrams
third <- function(input){
      temp <- filter(table3,phrase==input)
      if (is.na(temp[1,4])) {
            words <- unlist(strsplit(input,split = " "))
            new_input <- words[2]
            return(second(new_input))
      }
      else return(head(temp[,4],n=1))
}

##Predict fourth word using quadgrams
fourth <- function(input){
      temp <- filter(table4,phrase==input)
      if (is.na(temp[1,5])) {
            words <- unlist(strsplit(input,split = " "))
            new_input <- paste(words[2],words[3],sep = " ")
            return(third(new_input))
      }
      else return(head(temp[,5], n=1))
}

##Predict fifth word using pentagrams
fifth <- function(input){
      temp <- filter(table5,phrase==input)
      if (is.na(temp[1,6])) {
            words <- unlist(strsplit(input,split = " "))
            new_input <- paste(words[2],words[3],words[4],sep = " ")
            return(fourth(new_input))
      }
      else return(head(temp[,6],n=1))
}

#Function of prediction
##--------------------------------------------------
predictfun <- function(textin){
      token <- unlist(strsplit(textin, split = " "))
      ln_input <- length(token)
      if (ln_input == 1) return(second(textin))
      if(ln_input == 2) return(third(textin))
      if(ln_input == 3) return(fourth(textin))
      if(ln_input == 4) return(fifth(textin))
      if(ln_input >= 5){
            tmp <- token[(ln_input-3):ln_input]
            new_token <- paste(tmp[1],tmp[2],tmp[3],tmp[4],sep = " ")
            return(fifth(new_token))}}


function(input, output) {
      
      output$value <- renderText({
            paste("The next word as predicted is : ",predictfun(input$text))
      })

}