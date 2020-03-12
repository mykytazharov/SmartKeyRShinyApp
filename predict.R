#Read ngram tables. Define prediction function using stupid backoff algorithm.


#packages
library(stringr)
library(dplyr)
library(tm)

#read files
ngramFour <- readRDS("./fourgram.rds")
ngramThree <- readRDS("./trigram.rds")
ngramTwo <- readRDS("./bigram.rds")
ngramOne <- readRDS("./tdm.rds")




#train sets
#onegram
freqTerms <- findFreqTerms(ngramOne)
wordFrequency_one <- rowSums(as.matrix(ngramOne[freqTerms,]))
wordFrequency_one <- data.frame(n.gram=names(wordFrequency_one), frequency=wordFrequency_one, stringsAsFactors = FALSE)

#bigram
freqTerms <- findFreqTerms(ngramTwo)
wordFrequency_bi <- rowSums(as.matrix(ngramTwo[freqTerms,]))
wordFrequency_bi <- data.frame(n.gram=names(wordFrequency_bi), frequency=wordFrequency_bi)

#trigram
freqTerms <- findFreqTerms(ngramThree)
wordFrequency_tri <- rowSums(as.matrix(ngramThree[freqTerms,]))
wordFrequency_tri <- data.frame(n.gram=names(wordFrequency_tri), frequency=wordFrequency_tri)

#fourgram
freqTerms <- findFreqTerms(ngramFour)
wordFrequency_four <- rowSums(as.matrix(ngramFour[freqTerms,]))
wordFrequency_four <- data.frame(n.gram=names(wordFrequency_four), frequency=wordFrequency_four)

ngramOne <- wordFrequency_one[order(wordFrequency_one$frequency,decreasing = TRUE),]
ngramTwo <- wordFrequency_bi[order(wordFrequency_bi$frequency,decreasing = TRUE),]
ngramThree <- wordFrequency_tri[order(wordFrequency_tri$frequency,decreasing = TRUE),]
ngramFour <- wordFrequency_four[order(wordFrequency_four$frequency,decreasing = TRUE),]

row.names(ngramOne) <- NULL
row.names(ngramTwo) <- NULL
row.names(ngramThree) <- NULL
row.names(ngramFour) <- NULL


##code chunk to introduce vocabulary---in order to work with unseen words
 # index <- ngramOne$frequency < 2
 # ngramOne$n.gram[index] <- "UNK"
 # sum_unk<-sum(ngramOne$frequency[index])
 # ngramOne<-ngramOne[ngramOne$frequency >= 2, ]
 # vocabulary<-ngramOne
 # unk_row<-data.frame("UNK",sum_unk)
 # names(unk_row)<-c("n.gram","frequency")
 # 
 # ngramOne <- rbind(ngramOne, unk_row)

##additional filter of n-grams data frames
 # ngramOne <- filter(ngramOne, frequency > 0)
 # ngramTwo <- filter(ngramTwo, frequency > 1)
 # ngramThree<- filter(ngramThree, frequency > 1)
 ngramFour <- filter(ngramFour, frequency > 1)

 #save filtered data frames of n-grams
 saveRDS(ngramOne, file="onegram_final.rds")
 saveRDS(ngramTwo, file="bigram_final.rds")
 saveRDS(ngramThree, file="trigram_final.rds")
 saveRDS(ngramFour, file="fourgram_final.rds")

 
#test sets
ngramFour_test <- readRDS("./fourgram_test.rds")
ngramThree_test <- readRDS("./trigram_test.rds")
ngramTwo_test <- readRDS("./bigram_test.rds")
ngramOne_test <- readRDS("./tdm_test.rds") 
 
freqTerms <- findFreqTerms(ngramOne_test)
wordFrequency_one_test <- rowSums(as.matrix(ngramOne_test[freqTerms,]))
wordFrequency_one_test <- data.frame(n.gram=names(wordFrequency_one_test), frequency=wordFrequency_one_test)

freqTerms <- findFreqTerms(ngramTwo_test)
wordFrequency_bi_test <- rowSums(as.matrix(ngramTwo_test[freqTerms,]))
wordFrequency_bi_test <- data.frame(n.gram=names(wordFrequency_bi_test), frequency=wordFrequency_bi_test)

freqTerms <- findFreqTerms(ngramThree_test)
wordFrequency_tri_test <- rowSums(as.matrix(ngramThree_test[freqTerms,]))
wordFrequency_tri_test <- data.frame(n.gram=names(wordFrequency_tri_test), frequency=wordFrequency_tri_test)

freqTerms <- findFreqTerms(ngramFour_test)
wordFrequency_four_test <- rowSums(as.matrix(ngramFour_test[freqTerms,]))
wordFrequency_four_test <- data.frame(n.gram=names(wordFrequency_four_test), frequency=wordFrequency_four_test)

ngramOne_test <- wordFrequency_one_test[order(wordFrequency_one_test$frequency,decreasing = TRUE),]
ngramTwo_test <- wordFrequency_bi_test[order(wordFrequency_bi_test$frequency,decreasing = TRUE),]
ngramThree_test <- wordFrequency_tri_test[order(wordFrequency_tri_test$frequency,decreasing = TRUE),]
ngramFour_test <- wordFrequency_four_test[order(wordFrequency_four_test$frequency,decreasing = TRUE),]

row.names(ngramOne_test) <- NULL
row.names(ngramTwo_test) <- NULL
row.names(ngramThree_test) <- NULL
row.names(ngramFour_test) <- NULL


#function to get the last word of a string 
getLastWords <- function(string, words) {
   pattern <- paste("[a-z']+( [a-z']+){", words - 1, "}$", sep="")
   return(substring(string, str_locate(string, pattern)[,1]))
}

#stupid backoff algorithm implementation for the 4-gram language model

pred.word3 <- function(input.sentence){
   
   # take the last three words from input sentence as input for 4-gram table
   isr4 <- getLastWords(input.sentence, 3)
   # take the last two word from input sentence as input for 3-gram table
   isr3 <- getLastWords(isr4, 2)
   # take the last one word from input sentence as input for 2-gram table
   isr2 <- getLastWords(isr3, 1)
   
   # start at the 4-gram table
   pred.four <- ngramFour
   pred.four <- filter(ngramFour, grepl(paste0("^", isr4," "), n.gram))
   pred.four <- mutate(pred.four, predicted.word = getLastWords(n.gram, 1))
   pred.four <- mutate(pred.four, propability = frequency/ngramThree$frequency[ngramThree$n.gram == isr4])
   pred.four <- arrange(pred.four, desc(propability))
   pred.four <- select(pred.four, predicted.word, propability)
   pred.four <- top_n(pred.four, 3)
   
   
   # do exactly the same for the 3-gram table plus backoff factor
   pred.three <- ngramThree
   pred.three <- filter(pred.three, grepl(paste0("^", isr3," "), n.gram))
   pred.three <- mutate(pred.three, predicted.word = getLastWords(n.gram, 1))
   pred.three <- mutate(pred.three, propability = (frequency*0.4)/ngramTwo$frequency[ngramTwo$n.gram == isr3])
   pred.three <- arrange(pred.three, desc(propability))
   pred.three <- select(pred.three, predicted.word, propability)
   pred.three <- top_n(pred.three, 3) 
   
   
   # do exactly the same for the 2-gram table plus backoff factor
   pred.two <- ngramTwo
   pred.two <- filter(pred.two, grepl(paste0("^", isr2," "), n.gram))
   pred.two <- mutate(pred.two, predicted.word = getLastWords(n.gram, 1))
   if (length(ngramOne$frequency[ngramOne$n.gram == isr2]) != 0) {
   pred.two <- mutate(pred.two, propability = (frequency*0.4*0.4)/(ngramOne$frequency[ngramOne$n.gram == isr2]))
   } else {
      pred.two <- mutate(pred.two, propability = (frequency*0.4*0.4)/(31842))
   }
   pred.two <- arrange(pred.two, desc(propability))
   pred.two <- select(pred.two, predicted.word, propability)
   pred.two <- top_n(pred.two, 3) 
   
   # do exactly the same for the 1-gram table plus backoff factor
   pred.one <- ngramOne
   pred.one <- arrange(pred.one, desc(frequency))
   pred.one <- mutate(pred.one, predicted.word = n.gram)
   pred.one <- mutate(pred.one, propability = (frequency*0.4*0.4*0.4)/sum(ngramOne$frequency))
   pred.one <- arrange(pred.one, desc(propability))
   pred.one <- select(pred.one, predicted.word, propability)
   pred.one <- top_n(pred.one, 3) 
   
   # combine results into a dataframe and sort
   prop.table <- rbind(pred.four,pred.three,pred.two, pred.one) %>%
      arrange(desc(propability))  
   
   # drop words wich are found again in a lower N-gram
   prop.table <- prop.table[!duplicated(prop.table$predicted.word),]
   
   # show results
   prop.table <- prop.table %>%
      top_n(3)
   
   return(prop.table)
}



pred.word2 <- function(input.sentence){
   
   # take the last two words from input sentence as input for 3-gram table
   isr3 <- getLastWords(input.sentence, 2)
   # take the last  word from input sentence as input for 2-gram table
   isr2 <- getLastWords(isr3, 1)
   
   # start at the 3-gram table
   pred.three <- ngramThree
   pred.three <- filter(pred.three, grepl(paste0("^", isr3," "), n.gram))
   pred.three <- mutate(pred.three, predicted.word = getLastWords(n.gram, 1))
   pred.three <- mutate(pred.three, propability = frequency/ngramTwo$frequency[ngramTwo$n.gram == isr3])
   pred.three <- arrange(pred.three, desc(propability))
   pred.three <- select(pred.three, predicted.word, propability)
   pred.three <- top_n(pred.three, 3) 
      
   
   # do exactly the same for the 2-gram table plus backoff factor
   pred.two <- ngramTwo
   pred.two <- filter(pred.two, grepl(paste0("^", isr2," "), n.gram))
   pred.two <- mutate(pred.two, predicted.word = getLastWords(n.gram, 1))
   if (length(ngramOne$frequency[ngramOne$n.gram == isr2]) != 0) {
      pred.two <- mutate(pred.two, propability = (frequency*0.4)/(ngramOne$frequency[ngramOne$n.gram == isr2]))
   } else {
      pred.two <- mutate(pred.two, propability = (frequency*0.4)/(31842))
   }
   pred.two <- arrange(pred.two, desc(propability))
   pred.two <- select(pred.two, predicted.word, propability)
   pred.two <- top_n(pred.two, 3) 
   
   
   # do exactly the same for the 1-gram table plus backoff factor
   pred.one <- ngramOne
   pred.one <- arrange(pred.one, desc(frequency))
   pred.one <- mutate(pred.one, predicted.word = n.gram)
   pred.one <- mutate(pred.one, propability = (frequency*0.4*0.4)/sum(ngramOne$frequency))
   pred.one <- arrange(pred.one, desc(propability))
   pred.one <- select(pred.one, predicted.word, propability)
   pred.one <- top_n(pred.one, 3) 
   
   
   # combine results into a dataframe and sort
   prop.table <- rbind(pred.three,pred.two, pred.one) %>%
      arrange(desc(propability))  
   
   # drop words wich are found again in a lower N-gram
   prop.table <- prop.table[!duplicated(prop.table$predicted.word),]
   
   # show results
   prop.table <- prop.table %>%
      top_n(3)
   
   return(prop.table)
}

pred.word1 <- function(input.sentence){
   
   # take the last word from input sentence as input for 2-gram table
   isr2 <- getLastWords(input.sentence, 1)
   
   # start at the 3-gram table
   pred.two <- ngramTwo
   pred.two <- filter(pred.two, grepl(paste0("^", isr2," "), n.gram))
   pred.two <- mutate(pred.two, predicted.word = getLastWords(n.gram, 1)) 
   if (length(ngramOne$frequency[ngramOne$n.gram == isr2]) != 0) {
      pred.two <- mutate(pred.two, propability = frequency/ngramOne$frequency[ngramOne$n.gram == isr2])
   } else {
      pred.two <- mutate(pred.two, propability = (frequency)/(31842))
   }
   pred.two <- arrange(pred.two, desc(propability))
   pred.two <- select(pred.two, predicted.word, propability)
   pred.two <- top_n(pred.two, 3) 
   
   
   # do exactly the same for the 1-gram table plus backoff factor
   pred.one <- ngramOne
   pred.one <- arrange(pred.one, desc(frequency))
   pred.one <- mutate(pred.one, predicted.word = n.gram)
   pred.one <- mutate(pred.one, propability = (frequency*0.4)/sum(ngramOne$frequency))
   pred.one <- arrange(pred.one, desc(propability))
   pred.one <- select(pred.one, predicted.word, propability)
   pred.one <- top_n(pred.one, 3) 
   
   # combine results into a dataframe and sort
   prop.table <- rbind(pred.two, pred.one) %>%
      arrange(desc(propability))  
   
   # drop words wich are found again in a lower N-gram
   prop.table <- prop.table[!duplicated(prop.table$predicted.word),]
   
   # show results
   prop.table <- prop.table %>%
      top_n(3)
   
   return(prop.table)
}

# final function that chooses the appropiate backoff function depending on length of the input
#and performs cleaning of the input similar to one used when creating n-gram model
word.pred <- function(input){
   textInput <- tolower(input)
   textInput <- removePunctuation(textInput)
   textInput <- removeNumbers(textInput)
   textInput <- str_replace_all(textInput, "[^[:alnum:]]", " ")
   textInput <- stripWhitespace(textInput)
   inlength <- length(unlist(strsplit(textInput," ")))
   #additional code if we want to introduce vocabulary-- treat unseen words
   # words<-unlist(strsplit(textInput," "))
   # for (i in 1:length(words)){
   #    print(words[i] %in% vocabulary$n.gram)
   #    if ((words[i] %in% vocabulary$n.gram)==FALSE){ 
   #    textInput<- gsub(paste0("\\<", words[i], "\\>"), "UNK", textInput)
   #    }
   # }
   # print(textInput)
   if (inlength >= 3) {
      p <- pred.word3(textInput)
   }  else if (inlength == 2) {
      p <- pred.word2(textInput)
   }   else {p <- pred.word1(textInput)}
   
   return(p)
}


##test running time 

start_time <- Sys.time()
word.pred("hello my dear friend ")
end_time <- Sys.time()

end_time - start_time


#test accuracy using test data set

set.seed(2021)
testing.short <- ngramFour_test['n.gram']
testing.short<-testing.short[sample(nrow(testing.short), 1000), ]

# delete last word in string to get input sentence
input <- lapply(testing.short, function(x) gsub("\\s*\\w*$", "", x))

# save last true word that need to be predicted 
last.word.real <- lapply(testing.short, function(x) getLastWords(x, 1))

# apply predict function to input 
last.word.pred <- lapply(input , function(x) word.pred(x)[,1])

# create data frame that includes pass and fail 
accuracy.df <- as.data.frame(cbind(last.word.real, last.word.pred)) %>%
   mutate(last.word.real = as.character(last.word.real)) %>%
   mutate(last.word.pred = as.character(last.word.pred)) %>%
   mutate(pass = ifelse(str_detect(last.word.pred,last.word.real), 1, 0))

#outpu the results
head(accuracy.df)
sum(accuracy.df['pass'])/nrow(accuracy.df)

#garbage collection
gc()

#check the size of the data frame
utils:::format.object_size(object.size(ngramFour), "auto")

