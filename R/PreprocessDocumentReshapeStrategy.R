#==============================================================================#
#                       PreprocessDocumentReshapeStrategy                      #
#==============================================================================#
#' PreprocessDocumentReshapeStrategy
#'
#' \code{PreprocessDocumentReshapeStrategy} Class responsible for reshaping a Document class object into tokens, sentences, or paragraphs.
#'
#' Class responsible for reshaping a Document class object into tokens, sentences, or paragraphs.
#'
#' @template preprocessClasses
#' @template preprocessMethods
#' @template preprocessParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Preprocess Family of Classes
#' @export
PreprocessDocumentReshapeStrategy <- R6::R6Class(
  classname = "PreprocessDocumentReshapeStrategy",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = PreprocessDocumentStrategy0,

  private = list(

    reshapeSent = function(content) {

      s <- paste(content, collapse = " ")
      s <- NLP::as.String(s)
      sent_token_annotator <- openNLP::Maxent_Sent_Token_Annotator()
      a1 <- NLP::annotate(s, sent_token_annotator)
      sentences <- s[a1]
      return(sentences)
    }
  ),

  public = list(

    initialize = function(object, name = NULL) {

      private$..className <- "PreprocessDocumentReshapeStrategy"
      private$..methodName <- "initialize"
      private$..in <- object
      private$..logs <- LogR$new()

      if (private$validateParams()$code == FALSE) stop()

      # Create new Document object
      if (is.null(name)) name <- object$getName()
      private$..out <- Document$new(name = name)
      private$..out <- private$cloneDocument(private$..in, private$..out)

      # log
      private$..state <- paste0("Successfully initialized PreprocessDocumentReshapeStrategy class object.")
      self$logIt()

      invisible(self)
    },

    preprocess = function() {

      # Obtain content
      content <- private$..in$read()

      # Reshape content
      content <- private$reshapeSent(content)

      # Save content
      private$..out$content <- content

      # log
      private$..state <- paste0("Successfully reshaped Document into ",
                                private$..unit, " units.")
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
      visitor$preprocessDocumentReshapeStrategy(self)
    }
  )
)
