## ---- VReader
#==============================================================================#
#                                   VReader                                    #
#==============================================================================#
#' VReader
#'
#'
#' \code{VReader} Visitor class responsible for reading documents of various formats. Current formats include .csv, .Rdata, and .txt files
#'
#' \strong{VReader Class Overview:}
#' The VReader class is an implementation of the visitor design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This visitor pattern allows
#' new operations to be defined without changing the classes upon which
#' the visitor method operates.
#'
#' \strong{VReader Methods:}
#' The VReader class supports csv, Rdata, and text files through the following methods:
#'  \itemize{
#'   \item{\code{readCsv(file)}}{Method for reading csv files.}
#'   \item{\code{readRdata(file)}}{Method for reading Rdata files.}
#'   \item{\code{readText(file)}}{Method for reading text files.}
#' }
#'
#' @param file Object of the File family of classes
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Read/Write Classes
#' @export
VReader <- R6::R6Class(
  classname = "VReader",
  private = list(

    validateFile = function(document) {

      status <- list()
      status[['code']] <- TRUE

      path <- document$getPath()
      fileName <- document$getFileName()
      docType <- document$getDocType()

      if (length(path) == 0) {
        status[['code']] <- FALSE
        status[['msg']] <- paste("Unable to read document.",
                             "Path is missing with no default.",
                             "See ?VReader for assistance.")
        return(status)
      }

      if (length(fileName) == 0) {
        status[['code']] <- FALSE
        status[['msg']] <- paste("Unable to read document.",
                                 "File name is missing with no default.",
                                 "See ?VReader for assistance.")
        return(status)
      }

      if (!file.exists(file.path(path, fileName))) {
        status[['code']] <- FALSE
        status[['msg']] <- paste("Unable to read document.",
                                 "File name does not exist.",
                                 "See ?VReader for assistance.")
        return(status)
      }
      return(status)
    }
  ),
  public = list(

    readCsv = function(document) {

      status <- private$validateFile(document)
      if (status[['code']] == TRUE) {
        status[['msg']] <- paste("Successfully read", document$getName())
        status[['data']] <- read.csv(file.path(document$getPath(),
                                               document$getFileName()),
                                               header = header, stringsAsFactors = FALSE,
                                               sep = ",", quote = "\"'")
      }
      return(status)

    },

    readXlsx = function(document) {

      status <- private$validateFile(document)
      if (status[['code']] == TRUE) {
        status[['msg']] <- paste("Successfully read", document$getName())
        status[['data']] <- openxlsx::readWorkbook(xlsxFile =  file.path(document$getPath(),
                                               document$getFileName()))
      }
      return(status)

    },

    readRdata = function(file) {

      status <- private$validateFile(document)
      if (status[['code']] == TRUE) {
        status[['msg']] <- paste("Successfully read", document$getName())
        env <- new.env()
        status[['data']] <- load(file.path(document$getPath(),
                                           document$getFileName()), envir = env)
      }
      return(status)
    },

    readText = function(document) {

      status <- private$validateFile(document)
      if (status[['code']] == TRUE) {
        status[['msg']] <- paste("Successfully read", document$getName())
        con <- file(file.path(document$getPath(), document$getFileName()))
        on.exit(close(con))
        status[['data']] <- readLines(con)
      }
      return(status)
    }
  )
)
