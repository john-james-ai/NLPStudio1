#==============================================================================#
#                                 Document0                                         #
#==============================================================================#
#' Document0
#'
#' \code{Document0} Class defines data and methods for managing metadata for Corpus and Document objects.
#'
#' The metadata structures defined for Corpus and Document objects are based
#' upon the Dublin Core Document0data Initiative http://dublincore.org/documents/dcmi-terms/.
#' Four types of metadata are defined for Corpus and Document objects:
#' \itemize{
#'  \item CorpusDocument0Dublin DublinCoreCollections Dublin Core Document0data Initiative terms for document collections http://dublincore.org/groups/collections/collection-application-profile/
#'  \item DocumentDocument0Dublin Dublin Core Document0data Initiative terms for documents http://dublincore.org/documents/dcmi-terms/
#'  \item CorpusDocument0 An abbreviated list including eight terms from the Dublin document collection terms
#'  \item DocumentDocument0 An abbreviated list including six terms from the Dublin Core Document0data Element Set
#' }
#'
#' @section Methods:
#'  \describe{
#'   \item{\code{new()}}{Not implemented for this class}
#'   \item{\code{getCorpusDocument0()}}{Returns a nested list of the simplified and DCMI corpus metadata variables. }
#'   \item{\code{getDocumentDocument0()}}{Returns a nested list of the simplified and DCMI document metadata variables. }
#'  }
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Document0 <- R6::R6Class(
  classname = "Document0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(

    validateIO = function(io, path) {

      status <- list()
      status[['code']] <- TRUE

      if (!("IO0" %in% class(io))) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Unable to perform read operation.  Invalid ",
                                  "io parameter.  See ?", private$..className,
                                  " for further assistance. ")
        return(status)
      }
      return(status)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                              Instantiation                              #
    #-------------------------------------------------------------------------#
    initialize = function() { stop("This method is not implemented for this abstract class.") },

    #-------------------------------------------------------------------------#
    #                           Metadata Methods                              #
    #-------------------------------------------------------------------------#
    meta = function(key = NULL, value = NULL) {

      private$..methodName <- 'meta'

      # Return meta data
      if (is.null(key) & is.null(value)) {
        m <- Filter(Negate(is.null), private$..meta)
        return(as.data.frame(m))
      }

      private$..meta[[key]] <- value
      invisible(self)
    }
  )
)
