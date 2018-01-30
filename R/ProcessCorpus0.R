#==============================================================================#
#                                 ProcessCorpus0                               #
#==============================================================================#
#' ProcessCorpus0
#'
#' \code{ProcessCorpus0} Abstract superclass that defines the methods common to all Corpus Process family classes.
#'
#' This abstract defines the methods and interfaces common to all Process family classes which operate on Corpus objects. It inherits from the Process0 class.
#'
#' @template processClasses.R
#' @template processMethods.R
#' @template processParams.R
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Process Family of Classes
#' @export
ProcessCorpus0 <- R6::R6Class(
  classname = "ProcessCorpus0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Process0,

  private = list(
    cloneCorpus = function(inCorpus, outCorpus) {

      keys <- names(as.list(inCorpus$meta()))
      keys <- keys[keys!= "name"]
      values <- as.list(inCorpus$meta())
      values["name"] <- NULL
      outCorpus$content <- inCorpus$content
      lapply(seq_along(keys), function(k) {
        outCorpus$meta(key = keys[[k]], value = values[[k]])
      })

      return(outCorpus)
    }
  ),

  public = list(

    initialize = function(object, name, ...) { stop("This method is not implemented for this abstract class") },
    process = function() { stop("This method is not implemented for this abstract class") },
    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$processCorpus0(self)
    }
  )
)
