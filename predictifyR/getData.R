#==============================================================================#
#                                 getData                                      #
#==============================================================================#
#' getData
#'
#' \code{getData} Function obtains project data
#'
#' This function takes a URL as a parameter, downloads the data from
#' the website into the external directory for the studio and unzips the
#' data into the raw data directory
#'
#' @param studio The studio into which the data will be obtained
#' @param url The website from which the data will be downloaded
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
getData <- function(studio, url) {

  # # Download data
  # fm <- FileManager$new(studio)
  # status <- fm$download(studio = studio, url = url)
  # if (status[['code']] == TRUE) zipFilePath <- status[['data']]
  #
  # # Format extract directory
  # path <- studio$getPath()
  # exDir <- file.path(path, dirs$raw)
  #
  # # Extract files to unzip and designate exDir
  # files <- fm$unZipFile(zipFilePath = zipFilePath, exDir = exDir, listFiles = TRUE)
  # files <- files$msg$Name[grepl('en_US', files$msg$Name, fixed = TRUE) ]
  # files <- files[grepl('.txt', files, fixed = TRUE)]
  #
  # # Unzip data
  # ow <- getOption("warn")
  # options(warn = -1)
  # status <- fm$unZipFile(zipFilePath = zipFilePath, exDir = exDir, files = files)
  # options(warn = ow)

}
