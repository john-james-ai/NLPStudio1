#==============================================================================#
#                                 Analyzer                                     #
#==============================================================================#
#' Analyzer
#'
#' \code{Analyzer} Class performs analysis on Corpus objects through a series of visitors.
#'
#' @section Analyzer methods:
#' This section summarizes the methods in the Analyzer class.
#' \describe{
#'  \item{\code{summary(corpus)}}{Invokes the VAnalyzeSummary visitor class which collects summary statistics for a Corpus object.}
#'  \item{\code{diversity(corpus)}}{Dispatches VAnalyzeDiversity visitor class which reports several measures of lexical diversity.}
#' }
#'
#' @param corpus Corpus object
#'
#' @return A data frame with summary statistics for the Corpus.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Analyzer classes
#' @export
Analyzer <- R6::R6Class(
  "Analyzer",
  lock_class = FALSE,
  lock_objects = FALSE,

  public = list(

    #-------------------------------------------------------------------------#
    #                        Summary and Basic Analysis                       #
    #-------------------------------------------------------------------------#
    initialize = function() {
      invisible(self)
    },

    stats = function(corpus) {
      visitor <- VAnalyzeSummary$new()
      corpus$accept(visitor)
    },

    diversity = function(corpus) {
      visitor <- VAnalyzeDiversity$new()
      corpus$accept(visitor)
    }
  )
)
