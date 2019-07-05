# facebook_heartbeat
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
#' This function pulls the data from your messages folder created by the facebook_message_pull function.
#' The sentiment of the data is then calculated and a "heartbeat" chart is created. The chart spans the entire length of the
#' conversation that is present in your messenger data.
#'
#' \if{html}{\figure{heartbeat.png}{options: width=60\% alt="R logo"}}
#' \if{latex}{\figure{heartbeat.png}{options: width=0.5in}}
#'
#' @param data filepath to data
#' @keywords facebook_heartbeat
#' @return
#' @name facebook_heartbeat
#' @title facebook_heartbeat
#' @examples
#'
#' Load the facebookanalysis library
#'
#' library(facebookanalysis)
#'
#'   facebook_heartbeat(folder = 'messages')
#'
#' @source \url{http://nlp.stanford.edu/software/corenlp.shtml}
#' @export






facebook_heartbeat <- function(folder){

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
    # print(paste0(file_name," printed"))
    ##Get NRC Sentiment from the ith (f) text file
    ##If directory does not exist create directory. Else if it exists write .txt data to directory
    if(!dir.exists("./nrc_heartbeat")){
      dir.create("./nrc_heartbeat")
    }else{
      ##Create the file name from the filelist and name the .csv file this way
      # name<- gsub(pattern = './messages',replacement = "",x = file_name)
      # name <- gsub(pattern = ".txt*",replacement = "",x = name)
      # write.csv(x = values,file = paste0('./nrc_sentences/',name,' sentences.csv'))
    }
    s_v <- get_sentences(docs)
    s_v_sentiment <- get_sentiment(s_v)
    name<- gsub(pattern = './messages',replacement = "",x = file_name)
    name <- gsub(pattern = ".txt*",replacement = "",x = name)
    name <- gsub(pattern = "/",replacement = "",x = name)
    myfile_path<- file.path(".","nrc_heartbeat",paste0(name,"timeline heartbeat.pdf"))
    pdf(file = myfile_path)
    plot(
      s_v_sentiment,
      type="l",
      main= paste0(name," Messenger Timeline"),
      xlab = "Messenger Timeline",
      ylab= "Emotional Valence"
    )
    dev.off()

    myfile_path<- file.path(".","nrc_heartbeat",paste0(name,"timeline heartbeat.png"))
    png(file = myfile_path)
    plot(
      s_v_sentiment,
      type="l",
      main= paste0(name," Messenger Timeline"),
      xlab = "Messenger Timeline",
      ylab= "Emotional Valence",
      col = 'blue'
    )
    dev.off()
    print(paste0(name," facebook heartbeat charts created."))
  }


}
