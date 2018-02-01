#==============================================================================#
#                               PreprocessDocumentStrategy0                               #
#==============================================================================#
#' PreprocessDocumentStrategy0
#'
#' \code{PreprocessDocumentStrategy0} Abstract superclass that defines the methods common to all Document Process family classes.
#'
#' This abstract defines the methods and interfaces common to the Process family classes which operate on Document objects. It inherits from the Preprocess0 class.
#'
#' @template preprocessClasses
#' @template preprocessMethods
#' @template preprocessParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Preprocess Family of Classes
#' @export
PreprocessDocumentStrategy0 <- R6::R6Class(
  classname = "PreprocessDocumentStrategy0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = PreprocessStrategy0,

  private = list(
    cloneDocument = function(inDocument, outDocument) {

      keys <- names(as.list(inDocument$meta()))
      keys <- keys[keys!= "name"]
      values <- as.list(inDocument$meta())
      values["name"] <- NULL
      lapply(seq_along(keys), function(k) {
        outDocument$meta(key = keys[[k]], value = values[[k]])
      })

      return(outDocument)
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
      visitor$preprocessDocumentStrategy0(self)
    }
  )
)
