#------------------------------------------------------------------------------#
#                                Reshape                                       #
#------------------------------------------------------------------------------#
#' Reshape
#'
#' \code{Reshape}  Reshapes a corpus to a different level of aggregation.
#'
#' A wrapper for \code{\link[quanteda]{corpus_reshape}} that reshapes
#' the documents to a different level of aggregation. Units of 
#' aggregation can be defined as documents, paragraphs, or sentences. 
#' emojis with the words they represent.
#' Source \url{https://cran.r-project.org/web/packages/quanteda/quanteda.pdf}
#'
#' @usage Reshape$new(x, to = c("sentences", "paragraphs", "documents"), 
#' meta = TRUE, ...))$execute()
#'
#' @template textStudioParams
#' @param to Character string indicating the new level of aggregation 
#' @param useMeta Logical. If true, repeat the metadata values for each segmented 
#' text; if FALSE, drop the metadata in the segmented corpus.
#' @param ...additional arguments passed to tokens in the quanteda package. 
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{Reshape} Returns a corpus object with the documents 
#' defined as the new units, including document-level metadata identifying 
#' the original documents. 
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
Reshape <- R6::R6Class(
  classname = "Reshape",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,

  private = list(
    ..to = character(),
    ..useMeta = logical(),

    processText = function(content) {
      if (is.null(private$..emojis)) {
        content <- quanteda::corpus_reshape(x = content)
      } else {
        content <- textclean::replace_emoji(x = content,
                                            emoji_dt = private$..emojis)
      }
      return(content)
    }
  ),

  public = list(
    initialize = function(x, emojis = NULL) {
      private$..className <- "Reshape"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "Reshape"
      private$..x <- x
      private$..emojis <- emojis
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
