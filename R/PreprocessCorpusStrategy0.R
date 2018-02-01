#==============================================================================#
#                           PreprocessCorpusStrategy0                          #
#==============================================================================#
#' PreprocessCorpusStrategy0
#'
#' \code{PreprocessCorpusStrategy0} Abstract superclass that defines the methods common to all Corpus Preprocess family classes.
#'
#' This abstract defines the methods and interfaces common to all Preprocess family classes which operate on Corpus objects. It inherits from the Preprocess0 class.
#'
#' @template preprocessClasses
#' @template preprocessMethods
#' @template preprocessParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Preprocess Family of Classes
#' @export
PreprocessCorpusStrategy0 <- R6::R6Class(
  classname = "PreprocessCorpusStrategy0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = PreprocessStrategy0,

  private = list(
    cloneCorpus = function(inCorpus, outCorpus) {

      keys <- names(as.list(inCorpus$meta()))
      keys <- keys[keys!= "name"]
      values <- as.list(inCorpus$meta())
      values["name"] <- NULL
      lapply(seq_along(keys), function(k) {
        outCorpus$meta(key = keys[[k]], value = values[[k]])
      })

      return(outCorpus)
    }
  ),

  public = list(

    initialize = function(object, name = NULL, substitutions = NULL, splits = NULL) {
      stop("This method is not implemented for this abstract class")
    },
    preprocess = function() { stop("This method is not implemented for this abstract class") },
    getResult = function() { stop("This method is not implemented for this abstract class") },
    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$processCorpusStrategy0(self)
    }
  )
)
