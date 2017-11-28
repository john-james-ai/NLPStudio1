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

    validateFile = function(file, method, class) {

      f <- file$exposeObject()

      if (lengtH(f$path) == 0) {
        v <- Validator0$new()
        v$notify(class = "VReader", method = method, fieldName = "path",
                 level = "Error", value = "",
                 msg = paste("Unable to read document.",
                             "Path is missing with no default.",
                             "See ?VReader for assistance."),
                 expect = TRUE)
        stop()
      }

      if (lengtH(f$fileName) == 0) {
        v <- Validator0$new()
        v$notify(class = "VReader", method = method, fieldName = "fileName",
                 level = "Error", value = "",
                 msg = paste("Unable to read document.",
                             "File name is missing with no default.",
                             "See ?VReader for assistance."),
                 expect = TRUE)
        stop()
      }

      v <- ValidatorClass$new()
      if (v$validate(class = "VReader", method = method, fieldName = "class(file)",
                     level = "Error", value = class(file)[1],
                     msg = paste("Unable to read document. Object is not a",
                                 class, "class object.",
                                 "See ?VReader for assistance."),
                     expect = class) == FALSE) {
        stop()
      }

      if (!file.exists(file.path(f$path, f$fileName))) {
        v <- Validator0$new()
        v$notify(class = "VReader", method = method, fieldName = "path/fileName",
                 level = "Error", value = file.path(f$path, f$fileName),
                 msg = paste("Unable to read document.",
                             "File does not exist.",
                             "See ?VReader for assistance."),
                 expect = TRUE)
        stop()
      }
    }
  ),
  public = list(

    readCsv = function(file) {

      private$validateFile(file, method = "readCsv", class = "FileCsv")
      f <- file$exposeObject()
      content <- read.csv(file.path(f$path, f$fileName), header = header, stringsAsFactors = FALSE,
                          sep = ",", quote = "\"'")
      file$addContent(content)

    },

    readRdata = function(file) {

      private$validateFile(file, method = "readRdata", class = "FileRdata")
      f <- file$exposeObject()
      env <- new.env()
      content <- load(file.path(f$path, f$fileName), envir = env)
      file$addContent(env[[content]])

    },

    readText = function(file) {

      private$validateFile(file, method = "readText", class = "FileText")
      f <- file$exposeObject()
      con <- file(file.path(f$path, f$fileName))
      on.exit(close(con))
      content <- readLines(con)
      file$addContent(content)

    }
  )
)
