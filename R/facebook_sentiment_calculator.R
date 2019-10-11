# facebook_sentiment_calculator
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:

#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:   'Ctrl + Shift + T'
#' Facebook: Sentiment Calculator
#'
#' This function pulls the data from your messages folder created by the facebook_message_pull function.
#' The sentiment of the data is then calculated and written to a pdf file which has bar charts and a sentiment table.
#' The sentiments the data pulls are: surprise, disgust, sadness, anticipation, joy, fear, anger, and trust. The data also
#' pulls the positive and negative percentage of the messages as well.
#'
#'
#' @param text filepath to data
#' @aliases text
#' @keywords clean_text
#' @return Text that has been cleaned e.g. special characters and certain punctuation removed.
#' @name clean_text
#' @title clean_text
#' @usage clean_text(text)
#' @import tm
#' @import ggplot2
#' @import jsonlite
#' @import kableExtra
#' @import knitr
#' @import rmarkdown
#' @examples
#'
#' library(facebookanalysis)
#'
#'\dontrun{clean_text(text)}
#'
utils::globalVariables(c("clean_text", "removeWords","stopwords","Corpus","DirSource","get_sentences","get_sentiment","pdf","plot","dev.off","na.omit",
                         "png","head","write.table","write.csv","createWorkbook","addWorksheet","createStyle","addStyle","writeData","saveWorkbook",
                         "write.csv","get_nrc_sentiment","barplot","text","unzip","writeData"))


#' @export
clean_text <- function(text){
  text <- text
  cleaned <- tm::stripWhitespace(x = text)
  cleaned <- tm::removePunctuation(x = cleaned)
  clean   <- tm::removeWords(x = cleaned,words = stopwords(kind = 'en'))
  clean
  return(clean)
}


#' @param folder filepath to data
#' @keywords facebook_sentiment_calculator
#' @return sentiment analysis
#' @name facebook_sentiment_calculator
#' @title facebook_sentiment_calculator
#' @import tm
#' @import stringr
#' @import syuzhet
#' @usage facebook_sentiment_calculator(folder)
#' @usage messages
#' @examples
#'
#' library(facebookanalysis)
#'
#'\dontrun{facebook_sentiment_calculator(folder = data)}
#'
#' @export
facebook_sentiment_calculator <- function(folder){
  
  # require(tm,quietly = TRUE)
  # require(stringr,quietly = TRUE)
  # require(syuzhet,quietly = TRUE)
  
  filelist <- list.files(path = paste0("./",folder),pattern = '.txt', full.names = TRUE)
  # print(filelist)
  version_type <- version_replace(major = version$major,minor = version$minor)
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
    ##Create
    ###Removes any NA's in the sentiment data that will cause an error later on
    value <- value[is.na(value)] <- 0
    prop.table(value[,1:8])
    ##Create the sentiment scores table
    sentimentscores <- round(colSums(prop.table((value[,1:8])))*100,digits = 1)
    # print(sentimentscores)
    ##Create a dataframe that contains the sentiment scores
    sentimentscores <- as.data.frame(sentimentscores)
    ##Rename the column names of the sentiment scores
    colnames(sentimentscores) <- c("Percentages")
    
    ##Rename the row names of the sentiment scores
    Emotions <- c("anger","anticipation","disgust","fear","joy","sadness",
                  "surprise","trust")

    ##If directory does not exist create directory. Else if it exists write .txt data to directory
    if(!dir.exists("./nrc_sentiment")||!dir.exists('./image')){
      dir.create("./nrc_sentiment")
      dir.create("./image")
    }else{
      ##Create the file name from the filelist and name the .csv file this way
      name<- gsub(pattern = './messages',replacement = "",x = file_name)
      name <- gsub(pattern = ".txt*",replacement = "",x = name)
      # write.csv(x = sentimentscores,file = paste0('./nrc_sentiment/',name,'.csv'))
    }
    ###If Sentiment Scores are NA's print 'No bar plot created"
    if(is.na(sentimentscores$Percentages)){
      print("No bar plot created")
    }else{
      name<- gsub(pattern = './messages',replacement = "",x = file_name)
      name <- gsub(pattern = ".txt*",replacement = "",x = name)
      name <- gsub(pattern = "/",replacement = "",x = name)
      # Open a pdf file
      myfile_path<- file.path(".","image",paste0(name," Positive Vs Negative.pdf"))
      # file_two<- pdf(paste0("./image/",name," emotions.pdf"))
      # dir.create(dirname(file_two), showWarnings = FALSE)
      pdf(file = myfile_path)
      barplot_two<- barplot(
        sort(colSums(prop.table(value[, 9:10]))),
        cex.names = 0.7,
        las = 1,
        xlim = c(0,3),
        ylim = c(0,1),
        main = paste0(toupper(name)," Positive vs. Negative Sentiment"),
        col = "blue"
      )
      ##Add text to the barplot that has been created
      # text(barplot_two, 0, round(sort(colSums(prop.table(value[,9:10]))), 2),cex=1,pos=3) 
      # Close the pdf file
      dev.off() 
      myfile_path<- file.path(".","image",paste0(name," Emotional Sentiment.pdf"))
      pdf(file = myfile_path)
      #Barplot of Emotional Sentiment
      barplot_one<- barplot(
        sort(colSums(prop.table(value[, 1:8]))),
        cex.names = 0.7,
        las = 1,
        xlim = c(0,9),
        ylim = c(0,.40),
        main = paste0(toupper(name)," Emotional Sentiment by Word"),
        col = "lightgreen"
        
      )
      text(barplot_one, 0, round(sort(colSums(prop.table(value[, 1:8]))), 2),cex=1,pos=3) 
      dev.off() 
    }
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
    # print(paste0(name," facebook messages created."))
    
  }
  
  
}

