#------------------------------------------------------------------------------#
#                         Replace Internet Slang                               #
#------------------------------------------------------------------------------#
#' ReplaceInternetSlang
#'
#' \code{ReplaceInternetSlang}  Replace Internet Slang
#'
#' A wrapper for \code{\link[textclean]{replace_internet_slang}}
#' replaces internet slang.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceInternetSlang$new(x, slang = NULL, replacement = NULL, ignoreCase = TRUE)$execute()
#'
#' @template textCleanParams
#' @param slang A vector of slang strings to replace.
#' @param replacement A vector of strings with which to replace slang
#' @param ignoreCase Logical. If TRUE the case of slang will be ignored (replacement regardless of case). 
#' Applies to default internet slang only.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceInternetSlang} Returns a vector with internet slang replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceInternetSlang <- R6::R6Class(
  classname = "ReplaceInternetSlang",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,

  private = list(
    ..slang = character(),
    ..ignoreCase = logical(),

    processText = function(content) {
      content <- textclean::replace_internet_slang(x = content,
                                         slang = private$..slang,
                                         replacement = private$..replacement,
                                         ignore.case = private$..ignoreCase)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, slang = NULL, replacement = NULL, ignoreCase = TRUE) {
      private$..className <- "ReplaceInternetSlang"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceInternetSlang"
      private$..x <- x
      private$..slang <- slang
      private$..replacement <- replacement
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      
      if (private$validateParams()$code == FALSE) stop()
      
      invisible(self)
    },
    
    getParams = function() {
      input <- list(
        x = private$..x,
        pattern = private$..slang,
        replacement = private$..replacement,
        ignoreCase = private$..ignoreCase
      )
      return(input)
    },
    accept = function(visitor)  {
      visitor$replaceInternetSlang(self)
    }
  )
)
