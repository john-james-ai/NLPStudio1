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
    ..meta = list(
      title = NULL,
      subject = NULL,
      description  = NULL,
      language = NULL,
      creator  = NULL,
      dateCreated = NULL,
      source  = NULL,
      format  = NULL
    ),

    validatePath = function(path) {
      if (!file.exists(path)) {
        private$..state <- paste0("Unable to perform read operation.  Invalid ",
                                  "path parameter.  See ?", private$..className,
                                  " for further assistance. ")
        self$logIt("Error")
        stop()
      }
      return(path)
    },

    validateIO = function(io) {
      if (is.null(io)) {
        io <- IOFactory$new()$getIOStrategy(path)
      } else {
        if (!("IO0" %in% class(io))) {
          private$..state <- paste0("Unable to perform read operation.  Invalid ",
                                    "io parameter.  See ?", private$..className,
                                    " for further assistance. ")
          self$logIt("Error")
          stop()
        }
      }
      return(io)
    },

    validateContent = function(content = NULL){
      if (is.null(private$..content) & is.null(content)) {
        private$..state <- paste0("Unable to perform write operation.  Content ",
                                  "paramter missing with no default.  See ?",
                                  private$..className, " for further assistance. ")
        self$logIt("Error")
        stop()
      } else {
        if (!is.null(content)) {
          return(content)
        }
      }
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

      # If no parameters, return meta data if available, else the metadata names
      if (is.null(key) & is.null(value)) {
        m <- Filter(Negate(is.null), private$..meta)
        if (length(m) == 0) {
          return(names(private$..meta))
        } else {
          return(as.data.frame(m))
        }
      }

      private$..meta[[key]] <- value
      invisible(self)
    }
  )
)
