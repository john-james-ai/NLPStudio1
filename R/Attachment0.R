#==============================================================================#
#                               Attachment0                                    #
#==============================================================================#
#' Attachment0
#'
#' \code{Attachment0} Defines interface for all Document class attachment classes.
#'
#' Defines the standard methods for the three types of Attachment classes which
#' may be attached to Document class objects: (1) Text, (2) Data, and (3) Analyses.
#'
#' @section Attachment0 methods:
#'  \itemize{
#'   \item{\code{content()}}{Setter method for the Attachment object content.}
#'   \item{\code{attachId()}}{Returns the attachment identifier for the attachment.}
#'  }
#'  
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Attachment Classes
#' @export
Attachment0 <- R6::R6Class(
  classname = "Attachment0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..content = character()
  ),

  active = list(
    content = function(value) {

      if (missing(value)) {
        private$..meta[['user']] <- Sys.info()["user"]
        private$..meta[["accessed"]] <- Sys.time()
        return(private$..content)
      } else {
        private$..content <- value
        private$..meta[['user']] <- Sys.info()["user"]
        private$..meta[["modified"]] <- Sys.time()
        private$..meta[["accessed"]] <- Sys.time()
        private$..state <- "Updated data object content."
        self$logIt()
        invisible(self)
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function() { stop("This method is not implemented for this abstract class.")},

    attachId = function() private$..meta[["attachId"]]
  )
)
