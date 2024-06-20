# facebook_report
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:

#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:   'Ctrl + Shift + T'
#' Facebook: Report Generator
#'
#' This function pulls the data from your messages folder created by the facebook_message_pull function.
#' The sentiment of the data is then calculated and written to a pdf file which has bar charts and a sentiment table.
#' The sentiments the data pulls are: surprise, disgust, sadness, anticipation, joy, fear, anger, and trust. The data also
#' pulls the positive and negative percentage of the messages as well.
#'
#'
#' @param folder filepath to data
#' @aliases folder
#' @keywords facebook_report
#' @return A report that contains the emotions present in the facebook messages sent between participants. Max of 2                     participants. 
#' @name facebook_report
#' @title facebook_report
#' @usage facebook_report(folder)
#' @import tm
#' @import stringr
#' @import syuzhet
#' @import ggplot2
#' @import jsonlite
#' @import kableExtra
#' @import knitr
#' @import rmarkdown
#' @usage facebook_report(folder)
#' @examples
#'
#' library(facebookanalysis)
#'
#'\dontrun{facebook_report(folder = 'messages')}
#'
#' @export


utils::globalVariables(c("clean_text", "removeWords","stopwords","Corpus","DirSource","get_sentences","get_sentiment","pdf","plot","dev.off","na.omit",
                         "png","head","write.table","write.csv","createWorkbook","addWorksheet","createStyle","addStyle","writeData","saveWorkbook",
                         "write.csv","get_nrc_sentiment","barplot","text","unzip","writeData"))

facebook_report <- function(folder){
  
  filelist <- list.files(path = paste0("./",folder),pattern = '.txt', full.names = TRUE)
  version_type <- version_replace(major = version$major,minor = version$minor)
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
    value <- get_nrc_sentiment(docs)
    # print(value)
    ##Create
    value <- value[is.na(value)] <- 0
    prop.table(value[,1:8])
    ##Create the sentiment scores table
    sentimentscores <- round(colSums(prop.table((value[,1:8])))*100,digits = 1)
    ##Create a dataframe that contains the sentiment scores
    sentimentscores <- as.data.frame(sentimentscores)
    ##Rename the column names of the sentiment scores
    colnames(sentimentscores) <- c("Percentages")
    
    ##Rename the row names of the sentiment scores
    Emotions <- c("anger","anticipation","disgust","fear","joy","sadness",
                  "surprise","trust")
    
    
    ##If directory does not exist create directory. Else if it exists write .txt data to directory
    if(!dir.exists("./nrc_sentiment")){
      dir.create("./nrc_sentiment")
    }else{
      ##Create the file name from the filelist and name the .csv file this way
      name<- gsub(pattern = './messages',replacement = "",x = file_name)
      name <- gsub(pattern = ".txt*",replacement = "",x = name)
      # write.csv(x = sentimentscores,file = paste0('./nrc_sentiment/',name,'.csv'))
    }
    
    name<- gsub(pattern = './messages',replacement = "",x = file_name)
    name <- gsub(pattern = ".txt*",replacement = "",x = name)
    name <- gsub(pattern = "/",replacement = "",x = name)
    
    ##Render the rmarkdown report
    rmarkdown::render(input = paste0("~\\R\\win-library\\",version_type,"\\facebookanalysis\\rmd\\facebook.Rmd"),
                      ###Parameters used in the Rmarkdown PDF Report
                                                             params = list(table = value,
                                                             docs = docs,
                                                             sentiment = sentimentscores,
                                                             set_author = name),
                      output_file = paste0(name," Rmarkdown"),
                      output_dir = "nrc_sentiment",quiet = TRUE,clean = TRUE)
    ##Print out what facebook report has been rendered
    print(paste0("Facebook Analysis Report for ",paste0(name," pdf file created")))
    ##Print the name of the facebook sentiment scores being created
    print(paste0(name," facebook messages created."))
    
  }
  
  
}

