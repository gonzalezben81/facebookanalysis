# version_replace
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
#' @param major version of R
#' @aliases version$major
#' @param minor version of R
#' @aliases version$minor
#' @usage version_replace(major,minor)
#' @keywords version_replace
#' @name version_replace
#' @title version_replace
#' @examples
#'
#'
#'   version_replace(major = version$major,minor = version$minor)
#'
#' @export



version_replace <- function(major,minor){
  
  major <- major
  minor <- minor
  
  version_major<- major
  
  version_minor<- gsub("\\..*","",minor)
  
  version <- paste(version_major,version_minor,sep = '.',collapse = "")
  
  version<- as.name(version)
  
  return(version)
  
}
