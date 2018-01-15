#==============================================================================#
#                                   VAnalyzeSummary                            #
#==============================================================================#
#' VAnalyzeSummary
#'
#'
#' \code{VAnalyzeSummary} Visitor class responsible for computing summary statistics on a text document
#'
#' \strong{VAnalyzeSummary Methods:}
#' The VAnalyzeSummary methods are as follows:
#'  \itemize{
#'   \item{\code{corpus(object)}}{Computes summary statistics for a corpus.}
#' }
#'
#' @param object The Corpus object
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Analysis Visitor Classes
#' @export
VAnalyzeSummary <- R6::R6Class(
  classname = "VAnalyzeSummary",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    analyzeDocument = function(object) {
      name <- object$getName()
      content <- paste(object$read(), collapse = ' ')
      size <- as.numeric(object.size(content))
      sentences <- quanteda::nsentence(content)
      tokens <- quanteda::ntoken(content)
      types <- quanteda::ntype(tolower(content))
      df <- data.frame(Name = name,
                 Size = size,
                 Sentences = sentences,
                 Tokens = tokens,
                 Types = types,
                 stringsAsFactors = FALSE)
      return(df)
    }
  ),

  public = list(

    initialize = function() {
      invisible(self)
    },

    corpus = function(object) {
      documents <- object$getDocuments()
      analysis <- rbindlist(lapply(documents, function(d) {
        private$analyzeDocument(d)
      }))
      total <- data.frame(Name = 'Total',
                          Size = sum(analysis$Size),
                          Sentences = sum(analysis$Sentences),
                          Tokens = sum(analysis$Tokens),
                          Types = sum(analysis$Types),
                          stringsAsFactors = FALSE)
      analysis <- rbind(analysis, total)
      return(analysis)
    }
  )
)
