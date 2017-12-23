#==============================================================================#
#                              DMetaSimple                                     #
#==============================================================================#
#' DMetaSimple
#'
#' \code{DMetaSimple} Class for adding and manipulating user defined meta data.
#'
#'
#' @section DMetaSimple core methods:
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a document. Not implemented for the abstract class.}
#'   \item{\code{getName()}}{Method that returns the name of the DMetaSimple object. }
#'   \item{\code{getFileName()}}{Method for obtaining the document file name. .}
#'   \item{\code{getPath()}}{Method for obtaining the document path. }
#'  }
#'
#'
#' @param key Character string indicating the variable name
#' @param value Character string representing the variable value
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family DMetaSimple classes
#' @export
DMetaSimple <- R6::R6Class(
  classname = "DMetaSimple",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..name = character(),
    ..author = character(),
    ..description = character(),
    ..heading = character(),
    ..id = character(),
    ..language = character(),
    ..origin = character(),
    ..class = character()
  ),

  active = list(
    name = function(value = NULL) ifelse(is.null(value), private$..name, return(private$..name <- value)),
    author = function(value = NULL) ifelse(is.null(value), private$..author, return(private$..author <- value)),
    description = function(value = NULL) ifelse(is.null(value), private$..description, return(private$..description <- value)),
    heading = function(value = NULL) ifelse(is.null(value), private$..heading, return(private$..heading <- value)),
    id = function(value = NULL) ifelse(is.null(value), private$..id, return(private$..id <- value)),
    language = function(value = NULL) ifelse(is.null(value), private$..language, return(private$..language <- value)),
    origin = function(value = NULL) ifelse(is.null(value), private$..origin, return(private$..origin <- value)),
    class = function(value = NULL) ifelse(is.null(value), private$..class, return(private$..class <- value))
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                          Initialize Method                              #
    #-------------------------------------------------------------------------#
    initialize = function() {
      invisible(self)
    },

    meta = function(key = NULL, value) {

      private$..methodName <- 'meta'

      if (is.null(key)) {
        key <- names(value)
      }

      if (is.null(key)) {
        start <- length(private) + 1
        end <- start + ncol(as.data.frame(value)) - 1
        key <- paste("metaVar", seq(start, end),  sep = "")
      }
      private[[key]] <- value
    },


    print = function() {
      df <- as.data.frame(private)
      names(df) <- capitalize(names(df))
      print(df)
      return(df)
    }
  )
)
