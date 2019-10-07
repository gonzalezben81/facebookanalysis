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
#' The .csv file is names for the participant you have been messaging. The function also pulls the date and time from the Json file
#' and converts it and writes the Sender, Time, and Content of the message to an .xlsx file.
#'
#'
#' @param folder filepath to data
#' @keywords facebook_message_pull
#' @return facebook message conversation to a .csv file
#' @name facebook_message_pull
#' @title facebook_message_pull
#' @import openxlsx
#' @usage facebook_message_pull(folder)
#' @examples
#'
#' library(facebookanalysis)
#'
#'\dontrun{facebook_message_pull(folder = data)}
#'
#' @export
#'
#'
#'
#'
utils::globalVariables(c("clean_text", "removeWords","stopwords","Corpus","DirSource","get_sentences","get_sentiment","pdf","plot","dev.off",
                         "png","head","write.table","write.csv","createWorkbook","addWorksheet","createStyle","addStyle","writeData","saveWorkbook",
                         "write.csv","get_nrc_sentiment","barplot","text","unzip","writeData"))
#' @export
facebook_message_pull <- function(folder){

  requireNamespace("openxlsx", quietly = TRUE)

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

    text_sender<- text$messages$sender_name
    text_sender_two<- text$messages$sender_name[2]
    text_time<- as.list(head(as.POSIXct(as.integer(as.numeric(as.character(text$messages$timestamp_ms)) / 1000.0),
                                        origin='1970-01-01', tz="UTC")))

    text_content <- text$messages$content
    unique_sender<- unique(text_sender)
    name<- unique(text_sender)

    text_content <- text$messages$content

    text_all <- cbind(text_sender,text_time,text_content)

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
      write.table(x = text_all,file = paste0('./messages/',name,'.txt'))
      write.csv(x = text_all,file = paste0('./messages/',name,'_sentences.csv'))
      wb <- createWorkbook()
      addWorksheet(wb, paste0("Facebook Data"), gridLines = TRUE)

      sty <- createStyle(numFmt = "yyyy/mm/dd hh:mm:ss")
      addStyle(wb = wb,sheet =  1, style = sty,rows = 1, cols = 1, gridExpand = TRUE)
      writeData(wb = wb, "Facebook Data", text_all, startRow = 1,startCol = 1)
      saveWorkbook(wb, file = paste0("./messages/FacebookDataDownload",name,".xlsx"), overwrite = TRUE)
      # write.csv(x = unique_sender_two,file = paste0('./messages/',name,'_',text_sender_two,'.csv'))
    }

    print(paste0(name," facebook messages created."))
  }
  # return(message_content)
  # print(paste0(name," facebook messages created."))

}

utils::globalVariables(c(".obj1", "obj2"))
