#==============================================================================#
#                               ProcessDocument0                               #
#==============================================================================#
#' ProcessDocument0
#'
#' \code{ProcessDocument0} Abstract superclass that defines the methods common to all Document Process family classes.
#'
#' This abstract defines the methods and interfaces common to the Process family classes which operate on Document objects. It inherits from the Process0 class.
#'
#' @template processClasses.R
#' @template processMethods.R
#' @template processParams.R
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Process Family of Classes
#' @export
ProcessDocument0 <- R6::R6Class(
  classname = "ProcessDocument0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Process0,

  private = list(
    cloneDocument = function(inDocument, outDocument) {

      keys <- names(as.list(inDocument$meta()))
      keys <- keys[keys!= "name"]
      values <- as.list(inDocument$meta())
      values["name"] <- NULL
      outDocument$content <- inDocument$content
      lapply(seq_along(keys), function(k) {
        outDocument$meta(key = keys[[k]], value = values[[k]])
      })

      return(outDocument)
    }
  ),

  public = list(

    initialize = function(object, name, ...) { stop("This method is not implemented for this abstract class") },
    process = function() { stop("This method is not implemented for this abstract class") },
    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$processDocument0(self)
    }
  )
)
