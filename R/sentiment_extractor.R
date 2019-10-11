# sentiment_extractor
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:   'Ctrl + Shift + T'
#' Facebook: Message Conversation Extractor
#'
#' This function pulls the sentiment from the .txt files that contain the text you want to look at. The sentiment is then combined into a data frame with the participant's name and the emotions related to that conversation.
#'
#'
#' @param folder filepath to data
#' @keywords sentiment_extractor
#' @return Dataframe with sentiment extracted from each conversation.
#' @name sentiment_extractor
#' @title sentiment_extractor
#' @import openxlsx
#' @import tm
#' @import ggplot2
#' @import jsonlite
#' @import kableExtra
#' @import knitr
#' @import rmarkdown
#' @usage sentiment_extractor(folder)
#' @examples
#'
#' library(facebookanalysis)
#'
#'\dontrun{sentiment_extractor(folder = data)}
#'
#' @export

sentiment_extractor <- function(folder){
  
  filelist <- list.files(path = paste0("./",folder),pattern = '.txt', full.names = TRUE)
  number_files<- length(filelist)
  
  sentiment_list <- list()
  sentiment_name <- list()
  ##Create dummy data.frame
  sentimentscores <- data.frame(file=rep('',1))
  for(f in 1:length(filelist)){
    ##Find the file path to the folder the facebook messages are in
    text_file_path <- file.path(paste0("./",folder))  
    ##Create the corpus from the folder the messages are in
    document <- Corpus(DirSource(text_file_path[1]))  
    # print(document)
    ##Unlist the documents to allow for sentiment analysis
    docs <- unlist(document)
    ##Look at the ith (f) in the vector list
    docs <- docs[f]
    ##Get the file names
    file_name <- filelist[f]
    name<- gsub(pattern = './messages',replacement = "",x = file_name)
    name <- gsub(pattern = ".txt*",replacement = "",x = name)
    name <- gsub(pattern = "/",replacement = "",x = name)
    # print(paste0(file_name," printed"))
    ##Get NRC Sentiment from the ith (f) text file
    value <- get_nrc_sentiment(docs)
    ##Create
    ###Removes any NA's in the sentiment data that will cause an error later on 
    # prop.table(value[,1:8])
    ##Create the sentiment scores table
    sentimentscores <- round(colSums(prop.table((value[,1:8])))*100,digits = 1)
    # print(sentiment_name)
    sentiment_list[[f]] <- sentimentscores # add it to your list
    
    print(paste0(name," ",f," facebook messages created out of ",number_files," overall messages."))
    
  }
  
  for(f in 1:length(filelist)){
    ##Find the file path to the folder the facebook messages are in
    text_file_path <- file.path(paste0("./",folder))  
    file_name <- filelist[f]
    name<- gsub(pattern = './messages',replacement = "",x = file_name)
    name <- gsub(pattern = ".txt*",replacement = "",x = name)
    name <- gsub(pattern = "/",replacement = "",x = name)
    
    # print(sentiment_name)
    sentiment_name[[f]] <- name # add it to your list
    
  }
  
  big_data <-  do.call(rbind,sentiment_list)
  
  big_data_two <- do.call(rbind,sentiment_name)
  
  big_data <- cbind(big_data_two,big_data)
  
  big_data <- as.data.frame(big_data)
  
  colnames(big_data) <- c("Participant","Anger","Anticipation","Disgust","Fear","Joy","Sadness","surprise","Trust")
  
  return(big_data)
  
}