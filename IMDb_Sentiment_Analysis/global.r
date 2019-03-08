#GLOBAL.R
## Allocate memory
options(java.parameters = "-Xmx10g")

## clear console
cat("\014")

## clear global variables
rm(list=ls())

#load required packages##################################################
library(dplyr)
library(SnowballC)
library(slam)
library(RWeka)
library(Matrix)
library(rvest)
library(XML)
library(stringr)
library(stringi)
library(tidyverse)
library(stringr)
library(tm)
library(udpipe)
library(text2vec)
library(glmnet)
library(caret)
library(caTools)
library(tidytext)
library(widyr)
library(ggraph)
library(ggforce)
library(igraph)
library(syuzhet)
library(igraph)
library(shinycssloaders)
library(wordcloud2)
library(shiny)
library(shinythemes)
#load required packages##################################################
# packages_needed <- c('dplyr','SnowballC','slam','tm',
#                      'RWeka','Matrix','rvest',
#                      'XML','stringr','stringi',
#                      'tidyverse','stringr', 
#                      'udpipe','text2vec','glmnet',
#                      'caret', 'caTools','tidytext',
#                      'widyr','ggraph', 'ggforce',
#                      'igraph','syuzhet','igraph', 
#                      'shinycssloaders', 'wordcloud2',
#                      'shiny', 'shinythemes')
# for (i in packages_needed){
#   if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org", quiet=TRUE)
#   require(i, character.only=TRUE)
# }



#chunk_into_sentences######################################################
# input: review column
# output: sublists of review, split in to sentences .!?,<>
chunk_into_sentences <- function(text) {
  break_points <- c(1, as.numeric(gregexpr('[[:alnum:]][.!?,<>()]', text)[[1]]) + 1)
  sentences <- NULL
  for(i in 1:length(break_points)) {
    res <- substr(text, break_points[i], break_points[i+1]) 
    if(i>1) { sentences[i] <- sub('.', '', res) } else { sentences[i] <- res }
  }
  sentences <- sentences[sentences=!is.na(sentences)]
  if(length(break_points)==2) {sentences<-text}
  return(sentences)
}
# https://stackoverflow.com/questions/18712878/r-break-corpus-into-sentences


#cleanup_function_before######################################################
# input: Splitted review, list of items to delete the sentence before the keyword (however, but)
# output: Review list without the sentences before the keyword
cleanup_function_before <- function(this_review_splited, list_delete_before){
  text <- tolower(this_review_splited)
  text <- str_replace(text, "[.!?,]", "")
  logical_list<-rep(0, length(text))
  for (i in 1:length(list_delete_before)){
    m <- grepl(list_delete_before[i], text)
    logical_list<-logical_list|m
  }
  return_review <-this_review_splited[!lead(logical_list,default = FALSE)]
  # return_review <- paste(return_review, collapse = ' ') # combine sentences list into one review
  return(return_review)
}


#cleanup_function_after######################################################
# input: Splitted review, list of items to delete the sentence after the keyword (despite, in spite of)
# output: Review list without the sentences after the keyword
cleanup_function_after <- function(this_review_splited, list_delete_after){
  text <- tolower(this_review_splited)
  text <- str_replace(text, "[.!?,]", "")
  logical_list<-rep(0, length(text))
  for (i in 1:length(list_delete_after)){
    m <- grepl(list_delete_after[i], text)
    logical_list<-logical_list|m
  }
  return_review <-this_review_splited[!lag(logical_list,default = FALSE)]
  # return_review <- paste(return_review, collapse = ' ')# combine sentences list into one review
  return(return_review)
}

#cleanup_function_this_sentence######################################################
# input: Splitted review, list of items to delete the sentence if the keyword occurs (while)
# output: Review list without the sentences if the keyword occurs in the sentence
cleanup_function_this_sentence <- function(this_review_splited, list_delete_this_sentence){
  text <- tolower(this_review_splited)
  text <- str_replace(text, "[.!?,]", "")
  logical_list<-rep(0, length(text))
  for (i in 1:length(list_delete_this_sentence)){
    m <- grepl(list_delete_this_sentence[i], text)
    logical_list<-logical_list|m
  }
  return_review <-this_review_splited[!logical_list]
  # return_review <- paste(return_review, collapse = ' ')# combine sentences list into one review
  return(return_review)
}


#func_replace_emoji######################################################
# input: Splitted review, table to replace the emoticons with the emotion description
# output: Review list with emotion description instead of emoticons
func_replace_emoji<- function(text, emoji_table){
  clean_text <-stri_replace_all_fixed(text,
                                      pattern = emoji_table$emoji.chars,
                                      replacement = paste(emoji_table$emoji.descriptions," "),
                                      vectorize_all=FALSE)
  return(clean_text)
}


#elongated_words2######################################################
# input: Single word
# output: Boolean value if a word is elongated or not (Eg: Aweeesomeeee)
# Rule: A character coming together more than twice
elongated_words2 <- function(my_str){
  temp <- strsplit(my_str, "")[[1]]
  
  for(i in 1:length(temp)){
    count = 1
    if(i+1 <= length(temp)){
      for(j in (i+1):length(temp)){
        if(temp[i] == temp[j]){
          count = count + 1
          if(count>=3){
            return(T)
          }
        } else{
          break
        }
      }
    }
    
  }
  return(F)
  
}

#clean_elongated_words2######################################################
# input: Single word
# output: Clean elongated word (Eg: Aweeeeesome will become Aweesome)
# Rule: Reduce elongations with 2 character repetitions together only
clean_elongated_words2 <- function(my_str){
  temp <- strsplit(my_str, "")[[1]]
  my_word <- NA
  for(i in 1:length(temp)){
    if(is.na(my_word)){
      my_word <- temp[i]
    }
    if((i+1) <= length(temp)){
      if(temp[i] != temp[i+1]){
        my_word <- paste(c(my_word, temp[i+1]), collapse = "")
      } else if((i-1) >= 1){
        if(temp[i] != temp[i-1]){
          my_word <- paste(c(my_word, temp[i+1]), collapse = "")
        }
      }
      
    }
    
  }
  return(my_word)
  
}


#correct######################################################
# input: Single word
# output: Correct the spelling from dictionary (Eg: Aweesome will become Awesome)
# Rule: Closest distant word
correct <- function(word) {
  
  word = tolower(word)
  edit_dist <- adist(word, wordlist)
  c(wordlist[edit_dist <= min(edit_dist,2)],word)[1]
}


#clean_words######################################################
# input: Words in the review
# output: Rating value written in review comment on 10 (Eg: 3/5 will return 6)
clean_words <- function(words){
  
  num1 <- numeric(length(words))
  num2 <- numeric(length(words))
  
  for(j in 1:length(words)){
    words[j] <- gsub("[a-z]|[A-Z]", "" ,words[j])
    words[j] <- gsub("\\,", "." ,words[j])
    
    while((!grepl("^[0-9]",words[j])) | (!grepl("[0-9]$",words[j]))){
      words[j] <- gsub("^[[:punct:]]", "" ,words[j])
      words[j] <- gsub("[[:punct:]]$", "" ,words[j])
    }
    
    temp1 <- unlist(strsplit(words[j],'/'))[1]
    temp2 <- unlist(strsplit(words[j],'/'))[2]
    
    
    if(suppressWarnings(!is.na(as.numeric(temp1)))){
      num1[j] <- as.numeric(temp1)
    } else{
      
      temp1 <- unlist(strsplit(temp1,'&'))
      temp1 <- unlist(strsplit(temp1,'-'))
      temp1 <- unlist(strsplit(temp1,'$'))
      temp1 <- unlist(strsplit(temp1,'@'))
      temp1 <- unlist(strsplit(temp1,'!'))
      temp1 <- unlist(strsplit(temp1,'%'))
      temp1 <- unlist(strsplit(temp1,'\\('))
      temp1 <- unlist(strsplit(temp1,'\\+'))
      temp1 <- unlist(strsplit(temp1,'\\)'))
      temp1 <- unlist(strsplit(temp1,'\\*'))
      num1[j] <- mean(as.numeric(temp1))
    }
    
    if(suppressWarnings(!is.na(as.numeric(temp2)))){
      num2[j] <- as.numeric(temp2)
    } else{
      
      temp2 <- substr(temp2, 1, 2)
      num2[j] <- ifelse(temp2 != "10", as.numeric(substr(temp2,1,1)), as.numeric(temp2))
    }
  }
  
  res <- sum(num1)*10/sum(num2)
  
  if(res > 10 | is.nan(res)){
    return(NA)
  } else{
    return(res)
  }
  
}

#IsWordCap######################################################
# input: Single word
# output: Boolean value for capital words (Eg: COOL will return True)
IsWordCap<-function(word){
  if (word==toupper(word) & is.na(as.numeric(word)) & nchar(word)>=2 )
  {
    out<-TRUE
  }
  else {out<-FALSE}
  return(out)
}

#CountWordsCap######################################################
# input: Whole review
# output: Count all capital words (Eg: The movie was COOL and FABULOUS will return 2)
CountWordsCap<-function(review){
  review_clean<- str_replace_all(review, "[^[:alnum:]]", " ")
  review_split <- str_split(review_clean, boundary("word"))
  tf_vector<-unlist(sapply(unlist(review_split), IsWordCap))
  n<-sum(tf_vector)
  return(n)
}

#LoadToEnvironment######################################################
# input: RData to be loaded, in the new environment
# output: env with Rdata
LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}


#clean_reviews######################################################
# input: Review data with text column for reviews
# output: Multiple features added to the data as follow:
# elongated_words_freq - Elongated word frequency
# rating_words_value - Rating of movie written in the review comment
# elongated_sentiment - Average intensified valence for the elongated words
# exclamed_sentiment - Average intensified valence for the exclaimed words
# is_elongated - Boolean (If the review has elongated words)
# is_exclaimed - Boolean (If the review has exclaimed words)
# capital_freq - Capital words frequency
# is_capital - Boolean (If the review has capital words)
clean_reviews <- function(data, wordlist){
  
  mycorpus <- VCorpus(VectorSource(data$text))
  corpus_frame <- data.frame(text=unlist(sapply(mycorpus, `[`, "content")), stringsAsFactors=F)
  
  list_delete_before <- c("\\<however\\>","\\<but\\>")
  
  list_delete_after <- c("\\<despite\\>","\\<in spite of\\>")
  
  list_delete_this_sentence <- c("\\<while\\>")
  
  # replace emoji characters with their descriptions
  # https://unicode.org/Public/emoji/12.0/emoji-test.txt
  emoji_table<- read.csv("./Data/emoji_table.csv", header = TRUE)
  
  #run functions################################################################
  raw_review <- corpus_frame[,1]
  corpus_frame_split <- lapply(raw_review,chunk_into_sentences)
  corpus_frame_split <- lapply(corpus_frame_split,cleanup_function_before, list_delete_before)
  corpus_frame_split <- lapply(corpus_frame_split,cleanup_function_after, list_delete_after)
  corpus_frame_split <- lapply(corpus_frame_split,cleanup_function_this_sentence, list_delete_this_sentence)
  #combine sublists (sentences) back into one element per review
  clean_review <- lapply(corpus_frame_split, paste, collapse = ' ')
  #replace emoji
  clean_review <- lapply(clean_review, func_replace_emoji, emoji_table)
  #save output file
  mycorpus_clean <- VCorpus(VectorSource(clean_review))
  corpus_frame_clean <- data.frame(text=unlist(sapply(mycorpus_clean, `[`, "content")), stringsAsFactors=F)
  
  corpus_frame_clean$ratings <- data$ratings
  #corpus_frame_clean$sentiment <- data$count_pos
  corpus_frame_clean$sentiment <- NULL
  
  corpus_frame_clean$elongated_words <- NA
  corpus_frame_clean$cleaned_elongated_words <- NA
  corpus_frame_clean$elongated_words_freq <- NA
  
  for (i in 1:nrow(corpus_frame_clean)){
    words <- unlist(strsplit(corpus_frame_clean$text[i],' '))
    
    # Removes punctuations
    words = gsub("[[:punct:]]", " ", words)
    # Removes numbers
    words = gsub("[[:digit:]]", " ", words)
    
    words = gsub("\\s+"," ",words)
    
    words <- unlist(strsplit(words,' '))
    
    words <- sapply(words, elongated_words2)
    words <- names(words)[words == T]
    
    words <- tolower(words)
    
    cleaned_elongated_words <- sapply(words, clean_elongated_words2)
    
    #Concatenate back to a string
    corpus_frame_clean$elongated_words[i] <-
      paste(words, collapse=" ")
    
    corpus_frame_clean$cleaned_elongated_words[i] <-
      paste(cleaned_elongated_words, collapse=" ")
    
    corpus_frame_clean$elongated_words_freq[i] <-
      length(words)
  }
  
  corpus_frame_clean$Spelled_elongated_words <- NA
  
  
  for (i in 1:length(corpus_frame_clean$cleaned_elongated_words)){
    words <- unlist(strsplit(corpus_frame_clean$cleaned_elongated_words[i],' '))
    words <- as.character(sapply(words,correct))
    
    #Concatenate back to a string
    corpus_frame_clean$Spelled_elongated_words[i] <-
      paste(words, collapse=" ")
  }
  
  
  corpus_frame_clean$rating_words <- NA
  corpus_frame_clean$rating_words_value <- NA
  
  for (i in 1:nrow(corpus_frame_clean)){
    
    if(grepl("+[0-9]/[0-9]+", corpus_frame_clean$text[i])){
      words <- unlist(strsplit(corpus_frame_clean$text[i],' '))
      words <- words[grepl("*[0-9]/[0-9]*", words)]
      
      corpus_frame_clean$rating_words_value[i] <- clean_words(words)
      
      corpus_frame_clean$rating_words[i] <-
        paste(words, collapse=" ")
      
    }
    
    if(grepl("+[0-9] out of [0-9]+", corpus_frame_clean$text[i]
             ,ignore.case = T)){
      
      body_comment <- gsub(" out of ","/", corpus_frame_clean$text[i])
      
      words <- unlist(strsplit(body_comment,' '))
      words <- words[grepl("*[0-9]/[0-9]*", words)]
      
      corpus_frame_clean$rating_words_value[i] <- clean_words(words)
      
      corpus_frame_clean$rating_words[i] <-
        paste(words, collapse=" ")
      
    }
    
    
    if(grepl("+(two|three|four|five|six|seven|eight|nine|ten) out of (two|three|four|five|six|seven|eight|nine|ten)+", corpus_frame_clean$text[i]
             ,ignore.case = T)){
      
      body_comment <- gsub(" out of ","/", corpus_frame_clean$text[i])
      body_comment <- gsub("one","1", body_comment, ignore.case = T)
      body_comment <- gsub("two","2", body_comment, ignore.case = T)
      body_comment <- gsub("three","3", body_comment, ignore.case = T)
      body_comment <- gsub("four","4", body_comment, ignore.case = T)
      body_comment <- gsub("five","5", body_comment, ignore.case = T)
      body_comment <- gsub("six","6", body_comment, ignore.case = T)
      body_comment <- gsub("seven","7", body_comment, ignore.case = T)
      body_comment <- gsub("eight","8", body_comment, ignore.case = T)
      body_comment <- gsub("nine","9", body_comment, ignore.case = T)
      body_comment <- gsub("ten","10", body_comment, ignore.case = T)
      
      words <- unlist(strsplit(body_comment,' '))
      words <- words[grepl("*[0-9]/[0-9]*", words)]
      
      corpus_frame_clean$rating_words_value[i] <- clean_words(words)
      
      corpus_frame_clean$rating_words[i] <-
        paste(words, collapse=" ")
      
    }
    
    
  }
  
  
  corpus_frame_clean$exclamated_sentences <- NA
  
  for (i in 1:nrow(corpus_frame_clean)){
    
    sentences <- unlist(strsplit(corpus_frame_clean$text[i],'\\.'))
    
    sentences <- gsub("([!\\s])\\1+", "\\1", sentences, perl=TRUE)
    
    sentences <- unlist(strsplit(sentences,'(?<=[!])', perl = T))
    
    sentences <- sentences[grepl("+\\!+",sentences)]
    
    corpus_frame_clean$exclamated_sentences[i] <-
      paste(sentences, collapse=" .")
    
  }
  
  CommentsText <- sapply(corpus_frame_clean$exclamated_sentences
                         ,function(x) iconv(x, 'utf8', 'ascii',""))
  
  text <- tolower(CommentsText)
  
  ud_model <- udpipe_load_model("./Data/english-ewt-ud-2.3-181115.udpipe")
  
  pos_tokens <- udpipe_annotate(ud_model, x =text)
  pos_tokens <- as.data.frame(pos_tokens)
  pos_tokens <- subset(pos_tokens, pos_tokens$upos %in% c("ADJ", "VERB"))
  
  pos_tokens$doc_id <- gsub("doc","", pos_tokens$doc_id)
  pos_tokens$doc_id <- as.numeric(pos_tokens$doc_id)
  
  corpus_frame_clean$exclamated_words <- ""
  
  for(i in unique(pos_tokens$doc_id)){
    
    words <- subset(pos_tokens$lemma, pos_tokens$doc_id == i)
    
    corpus_frame_clean$exclamated_words[i] <-
      paste(words, collapse=" ")
  }
  
  
  
  
  dictionary <- read.csv("./Data/SentimentDictionary.csv")
  dictionary[,2:4] <- sapply(dictionary[,2:4],function(x) x-5)
  
  corpus_frame_clean$elongated_sentiment <- NA
  
  for (i in 1:nrow(corpus_frame_clean)){
    
    if(corpus_frame_clean$Spelled_elongated_words[i] != ""){
      
      split <- strsplit(corpus_frame_clean$Spelled_elongated_words[i],split=" ")[[1]] 
      
      m <- match(split, dictionary$Word)
      
      present <- !is.na(m)
      
      wordvalences <- dictionary$VALENCE[m[present]] * 2
      
      corpus_frame_clean$elongated_sentiment[i] <- mean(wordvalences, na.rm=TRUE)
      
      if (is.na(corpus_frame_clean$elongated_sentiment[i])) 
        corpus_frame_clean$elongated_sentiment[i] <- 0 
      else corpus_frame_clean$elongated_sentiment[i] <- corpus_frame_clean$elongated_sentiment[i]
    }
  }
  
  
  corpus_frame_clean$exclamed_sentiment <- NA
  
  for (i in 1:nrow(corpus_frame_clean)){
    
    if(corpus_frame_clean$exclamated_words[i] != ""){
      
      split <- strsplit(corpus_frame_clean$exclamated_words[i],split=" ")[[1]] 
      
      m <- match(split, dictionary$Word)
      
      present <- !is.na(m)
      
      wordvalences <- dictionary$VALENCE[m[present]] * 2
      
      corpus_frame_clean$exclamed_sentiment[i] <- mean(wordvalences, na.rm=TRUE)
      
      if (is.na(corpus_frame_clean$exclamed_sentiment[i])) 
        corpus_frame_clean$exclamed_sentiment[i] <- 0 
      else corpus_frame_clean$exclamed_sentiment[i] <- corpus_frame_clean$exclamed_sentiment[i]
    }
  }
  
  
  corpus_frame_clean$elongated_words <- NULL
  corpus_frame_clean$cleaned_elongated_words <- NULL
  corpus_frame_clean$Spelled_elongated_words <- NULL
  corpus_frame_clean$rating_words <- NULL
  corpus_frame_clean$exclamated_sentences <- NULL
  corpus_frame_clean$exclamated_words <- NULL
  corpus_frame_clean$ratings <- NULL
  
  corpus_frame_clean$is_elongated <- ifelse(corpus_frame_clean$elongated_words_freq > 0, 1, 0)
  #corpus_frame_clean$is_rated <- ifelse(!is.na(corpus_frame_clean$rating_words_value),1, 0)
  corpus_frame_clean$is_exclaimed <- ifelse(!is.na(corpus_frame_clean$exclamed_sentiment),1, 0)
  
  corpus_frame_clean[is.na(corpus_frame_clean)] <- 0
  
  corpus_frame_clean <- corpus_frame_clean[!is.na(corpus_frame_clean$text),]
  
  review_comments<-corpus_frame_clean$text
  FreqCapWords<-sapply(review_comments,CountWordsCap)
  
  corpus_frame_clean$capital_freq <- as.vector(FreqCapWords)
  corpus_frame_clean$is_capital <- ifelse(corpus_frame_clean$capital_freq > 0, 1, 0)
  
  return(corpus_frame_clean)
  
}

#create_features_from_model######################################################
# input: data- Data frame with reviews, model- TFIDF model used while training, vectorizer- Training vocabulary word vector, lsa- LSA model used while training
# output: TFIDF, LSA and all the features combind in a matrix which is used for modelling
create_features_from_model <- function(data, model, vectorizer, lsa){
  
  data$text <- data %$%
    str_to_lower(text) %>%
    str_replace_all("[^[:alpha:]]", " ") %>%
    str_replace_all("\\s+", " ")
  
  data_tokens <- itoken(data$text, 
                        preprocessor = tolower, tokenizer = word_tokenizer, progressbar = FALSE)
  data_dtm <- create_dtm(data_tokens, vectorizer)
  data_tfidf <- transform(data_dtm, model)
  
  data_lsa <- transform(data_tfidf, lsa)

  res <- data %>%
    select(-c(text)) %>%
    sparse.model.matrix(~ . - 1, .) %>%
    cbind(data_tfidf, data_lsa)
  
  return(res)
}

#model_creation######################################################
# input: training data
# output: List of variables which will be used to transfer information to the testing data
# tfidf_model- TFIDF model used while training, vectorizer- Training vocabulary word vector, m_lsa- LSA model used while training, train_tfidf- Training TFIDF
model_creation <- function(train){
  
  train$text <- train %$%
    str_to_lower(text) %>%
    str_replace_all("[^[:alpha:]]", " ") %>%
    str_replace_all("\\s+", " ")
  
  train_tokens <- itoken(train$text, 
                         preprocessor = tolower, tokenizer = word_tokenizer, progressbar = FALSE)
  vectorizer <- create_vocabulary(train_tokens, ngram = c(1, 1), stopwords = stopwords::stopwords("en")) %>%
    prune_vocabulary(term_count_min = 3, doc_proportion_max = 0.5, vocab_term_max = 4000) %>%
    vocab_vectorizer()
  
  train_dtm <- create_dtm(train_tokens, vectorizer)
  tfidf_model <- TfIdf$new(norm = "l2", sublinear_tf = T)
  train_tfidf <- fit_transform(train_dtm, tfidf_model)
  
  m_lsa <- LSA$new(n_topics = 25, method = "randomized")
  lsa <- fit_transform(train_tfidf, m_lsa)
  
  return(list(tfidf_model, vectorizer, m_lsa, train_tfidf))
}

#sentiment_prediction######################################################
# input: test- Data frame with reviews under prediction, tr_model- output for model_creation for training data, 
#   glmnet_clas- Logistic net Model created from training data
# output: sentiment (Positve or Negative)
sentiment_prediction <- function(test, tr_model, glmnet_clas){
  
  test_indep <- create_features_from_model(test, model = tr_model[[1]], vectorizer = tr_model[[2]], tr_model[[3]])
  
  # predict with models
  pred <- predict(glmnet_clas, test_indep, type = 'response')[,1]
  
  sentiment <- ifelse(pred>0.5,"Positive","Negative")
  
  return(sentiment)
  
}

#create_tdm_df######################################################
# input: data- Data frame with reviews
# output: Data frame of term document matrix sorted of maximum frequency
create_tdm_df <- function(data){
  # Load the data as a corpus
  docs <- Corpus(VectorSource(data$text))
  #inspect(docs)
  
  #Replacing “/”, “@” and “|” with space
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  # Cleaning the data
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, c(stopwords("english"),"film","movie"))
  # Remove your own stop word
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  # Build a term-documment matrix
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  df <- data.frame(word = names(v),freq=v)
  
  return(df)
}

#create_tfidf_df######################################################
# input: data- Data frame with reviews
# output: Data frame of TFIDF sorted of maximum TFIDF frequency
create_tfidf_df <- function(data){
  # Load the data as a corpus
  docs <- Corpus(VectorSource(data$text))
  #inspect(docs)
  
  #Replacing “/”, “@” and “|” with space
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  # Cleaning the data
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, c(stopwords("english"),"film","movie"))
  # Remove your own stop word
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  tf_idf <- TermDocumentMatrix(docs, control = list(weighting = weightTfIdf))
  
  m <- as.matrix(tf_idf)
  v <- sort(rowSums(m),decreasing=TRUE)
  df <- data.frame(word = names(v),freq=v)
  
  return(df)
}



#tdm.TFIDF######################################################
# input: Input review text
# output: TFIDF Matrix
tdm.TFIDF <- function(input_text){
  
  text_corpus = Corpus(VectorSource(input_text$text))
  
  # Text_corpus is a collection of tweets where every tweet is a document
  tdm <- DocumentTermMatrix(text_corpus, control = list(weighting = weightTfIdf))
  
  return(tdm)
}

#getSentiments.TF_IDF.nrc######################################################
# input: TFIDF Matrix
# output: Sentiment emotions score for sadness, fear, disgust, anger, trust, surprise, joy, anticipation
getSentiments.TF_IDF.nrc <- function(tdm.tfidf){
  
  m <- as.matrix(tdm.tfidf)
  
  word_tfidf <- sort(colSums(m), decreasing = TRUE)
  dm <- data.frame(word = names(word_tfidf), tfidf = word_tfidf)
  dm.subset <- dm[dm$tfidf>=quantile(dm$tfidf,0.25),]
  nrc.lex <- get_nrc_sentiment(as.character(dm.subset$word))
  
}
