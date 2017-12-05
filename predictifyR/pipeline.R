#==============================================================================#
#                                 pipeline                                     #
#==============================================================================#
#' pipeline
#'
#' \code{pipeline} Runs the predictifyR pipeline
#'
#' This function runs the pipeline from predictifyR word prediction application
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
pipeline <- function() {

  # Get Data
  url <-  'http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
  getData(blue, url)
}
