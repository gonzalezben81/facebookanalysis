# facebook_sentences_pull
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
#' Facebook: Sentiment Calculator
#'
#' This function pulls the sentences from your messages folder created by the facebook_message_pull function.
#' The sentences are then placed in a csv file and written to the nrc_sentences folder that is created by the function.
#'
#'
#' @param data filepath to data
#' @keywords facebook_sentences_pull
#' @return facebook sentences
#' @name facebook_sentences_pull
#' @title facebook_sentences_pull
#' @examples
#' #' @source \url{http://lib.stat.cmu.edu/datasets/detroit}
#' Load the facebookanalysis library
#'
#' library(facebookanalysis)
#'
#'   facebook_sentences_pull(folder = 'messages')
#'
#' @export

facebook_pull_sentences <- function(folder){

  require(tm,quietly = TRUE)
  require(stringr,quietly = TRUE)
  require(syuzhet,quietly = TRUE)

  filelist <- list.files(path = paste0("./",folder),pattern = '.txt', full.names = TRUE)
  # print(filelist)
  for(f in 1:length(filelist)){
    ##Find the file path to the folder the facebook messages are in
    text_file_path <- file.path(paste0("./",folder))
    ##Create the corpus from the folder the messages are in
    document <- Corpus(DirSource(text_file_path[1]))
    ##Unlist the documents to allow for sentiment analysis
    docs <- unlist(document)
    ##Look at the ith (f) in the vector list
    docs <- docs[f]
    ##Get the file names
    file_name <- filelist[f]
    ##Get NRC Sentiment from the ith (f) text file
    values <- get_sentences(docs)

    ##If directory does not exist create directory. Else if it exists write .txt data to directory
    if(!dir.exists("./nrc_sentences")){
      dir.create("./nrc_sentences")
    }else{
      ##Create the file name from the filelist and name the .csv file this way
      name<- gsub(pattern = './messages',replacement = "",x = file_name)
      name <- gsub(pattern = ".txt*",replacement = "",x = name)
      write.csv(x = values,file = paste0('./nrc_sentences/',name,' sentences.csv'))
    }

    print(paste0(name," facebook sentences csv created."))

  }


}