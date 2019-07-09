# facebook_message_pull
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
#' This function pulls the sentences from your messages folder and places them in a handy .csv file for analysis.
#' The participants, timestamp, and content of the messages are pulled out and placed in the .csv file that is created.
#' The .csv file is names for the participant you have been messaging.
#'
#'
#' @param folder filepath to data
#' @keywords facebook_message_pull
#' @return facebook message conversation to a .csv file
#' @name facebook_message_pull
#' @title facebook_message_pull
#' @examples
#' #' @source \url{http://lib.stat.cmu.edu/datasets/detroit}
#' Load the facebookanalysis library
#'
#' library(facebookanalysis)
#'
#'   facebook_message_pull(folder = 'messages')
#'
#' @export
#'
#'
#'
#'


facebook_message_pull <- function(folder){


  filelist <- list.files(path = folder, full.names = TRUE)

  for(f in filelist){

    text<- jsonlite::fromJSON(txt = paste0(f,'/message_1.json'),simplifyDataFrame = TRUE)

    text_messages<- text$messages

    message_content<- as.character(text_messages$content)

    # text_frame<- jsonlite::fromJSON(txt = paste0(f,'/message_1.json'))

    # text$participants$name
    # text$thread_type
    # text$thread_path
    # text$participants[1,1]
    # text$participants[2,1]
    # message_frame<- as.data.frame(text_frame$messages)

    text_sender<- text$messages$sender_name[1]
    text_sender_two<- text$messages$sender_name[2]
    print(paste0('Text Sender one',text_sender))
    print(paste0('Text Sender two',text_sender_two))
    text_content <- text$messages$content
    unique_sender<- unique(text_sender)
    name<- unique(text_sender)

    text_content <- text$messages$content

    text_all <- cbind(text_sender,text_content)

    unique_sender_one <- text_all[ which(text_all[,1]==unique_sender[1]), ]
    # unique_sender_one <- as.data.frame(unique_sender_one)
    # colnames(unique_sender_one) <- c(text_sender,'Message Content')

    unique_sender_two <- text_all[ which(text_all[,1]==unique_sender[2]), ]
    # unique_sender_two <- as.data.frame(unique_sender_two)
    # colnames(unique_sender_two) <- c(text_sender_two,'Message Content')
    # all_unique <- rbind(unique_sender_one,unique_sender_two)

    ###If directory does not exist create directory. Else if it exists write .txt data to directory
    if(!dir.exists("./messages")){
      dir.create("./messages")
    }else{
      name<- gsub(pattern = './facebookmessages/messages/inbox/',replacement = "",x = f)
      name <- gsub(pattern = "_.*",replacement = "",x = name)
      write.table(x = message_content,file = paste0('./messages/',name,'.txt'))
      write.csv(x = unique_sender_one,file = paste0('./messages/',name,'_sentences.csv'))
      write.csv(x = unique_sender_two,file = paste0('./messages/',name,'_',text_sender_two,'.csv'))
    }

    print(paste0(name," facebook messages created."))
  }
  # return(message_content)
  # print(paste0(name," facebook messages created."))

}


