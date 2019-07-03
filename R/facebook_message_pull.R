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
#' Facebook: Message Pull
#'
#' This function pulls the data from the Facebook zip folder that is downloaded when you retrieve the contents of your data.
#' Then the data is placed inside of a message folder
#'
#'
#' @param data filepath to data
#' @keywords facebook_message_pull
#' @return
#' @name facebook_message_pull
#' @title facebook_message_pull
#' @examples
#' Load the facebookanalysis library
#'
#' library(facebookanalysis)
#'
#'   facebook_message_pull(folder = './facebookmessages/messages/inbox/')
#'
#' @export

facebook_message_pull <- function(folder){


  filelist <- list.files(path = folder, full.names = TRUE)

  for(f in filelist){

    text<- jsonlite::fromJSON(txt = paste0(f,'/message_1.json'),simplifyDataFrame = TRUE)

    text_messages<- text$messages

    message_content<- as.character(text_messages$content)

    text_sender<- text$messages$sender_name

    text_content <- text$messages$content
    unique_sender<- unique(text_sender)


    text_content <- text$messages$content

    text_all <- cbind(text_sender,text_content)

    unique_sender_one <- text_all[ which(text_all[,1]==unique_sender[1]), ]

    unique_sender_two <- text_all[ which(text_all[,1]==unique_sender[2]), ]

    ###If directory does not exist create directory. Else if it exists write .txt data to directory
    if(!dir.exists("./messages")){
      dir.create("./messages")
    }else{
      name<- gsub(pattern = './facebookmessages/messages/inbox/',replacement = "",x = f)
      name <- gsub(pattern = "_.*",replacement = "",x = name)
      write.table(x = message_content,file = paste0('./messages/',name,'.txt'))
      write.csv(x = unique_sender_one,file = paste0('./messages/',unique_sender[1],'.csv'))
      write.csv(x = unique_sender_two,file = paste0('./messages/',unique_sender[2],'.csv'))
    }

    print(paste0(name," facebook messages created."))

  }
  # return(message_content)
  # print(paste0(name," facebook messages created."))

}
