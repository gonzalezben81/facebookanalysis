# facebook_unzip
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
#' Facebook: Zip folder extractor
#'
#' This function unzips the Facebook zip folder that is downloaded when you retrieve the contents of your data.
#' Then the data is placed inside of a facebookmessages folder
#'
#'
#' @param data filepath to data
#' @keywords facebook_unzip
#' @return This function pulls the data from your downloaded Facebook zip file.
#' @name facebook_unzip
#' @title facebook_unzip
#' @examples
#' Load the facebookanalysis library
#'
#' library(facebookanalysis)
#'
#'   facebook_unzip(path = "./facebook.zip")
#'
#' @export


facebook_unzip <- function(path){
##If directory does not exist create directory. Else if it exists write .txt data to directory
if(!dir.exists("./facebookmessages")){
  dir.create("./facebookmessages")
  unzip(zipfile = path,exdir = 'facebookmessages')
  print("Facebook Messages Directory Created and Files Unzipped")
}else{
  unzip(zipfile = path,exdir = 'facebookmessages')
  print("Facebook Messages Directory Created and Files Replaced")
}

}
