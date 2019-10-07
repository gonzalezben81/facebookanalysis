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
#' @param major major version of R
#' @param minor minor version of R
#' @keywords version_replace
#' @name version_replace
#' @title version_replace
#' @examples
#'
#'
#'   version_replace(major = 'major',minor = 'minor')
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
