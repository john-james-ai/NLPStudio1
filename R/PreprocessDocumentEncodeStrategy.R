#==============================================================================#
#                       PreprocessDocumentEncodeStrategy                             #
#==============================================================================#
#' PreprocessDocumentEncodeStrategy
#'
#' \code{PreprocessDocumentEncodeStrategy} Class responsible for performing encoding repairs in Document objects.
#'
#' Class corrects common encoding errors and converts UTF-8 to ASCII encoding. Replaces
#'
#' @template preprocessClasses
#' @template preprocessMethods
#' @template preprocessParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Preprocess Family of Classes
#' @export
PreprocessDocumentEncodeStrategy <- R6::R6Class(
  classname = "PreprocessDocumentEncodeStrategy",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = PreprocessDocumentStrategy0,

  public = list(

    initialize = function(object, name = NULL, substitutions = NULL) {

      private$..className <- "PreprocessDocumentEncodeStrategy"
      private$..methodName <- "initialize"
      private$..in <- object
      private$..logs <- LogR$new()

      if (private$validateParams()$code == FALSE) stop()

      if (is.null(substitutions)) {
        private$..substitutions <- NLPStudio:::encodings
      } else {
        private$..substitutions <- substitutions
      }

      # Create new Document object
      if (is.null(name)) name <- object$getName()
      private$..out <- Document$new(name = name)
      private$..out <- private$cloneDocument(private$..in, private$..out)

      # log
      private$..state <- paste0("Successfully initialized PreprocessDocumentEncodeStrategy class object.")
      self$logIt()

      invisible(self)
    },

    preprocess = function() {

      # Obtain content
      content <- private$..in$read()

      # Convert encoding
      Encoding(content) <- "latin1"
      content <- enc2utf8(content)

      # Repair content
      for (i in 1:nrow(private$..substitutions)) {
        content <- gsub(private$..substitutions[[1]][i], private$..substitutions[[2]][i], content)
      }

      # convert UTF-8 to ASCII
      private$..out$content <- iconv(content, "UTF-8", "ASCII", sub = "")

      # log
      private$..state <- paste0("Successfully performed PreprocessDocumentEncodeStrategy.")
      self$logIt()

      invisible(self)
    },


    getResult = function() {
      return(private$..out)
    },

    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$preprocessDocumentEncodeStrategy(self)
    }
  )
)
