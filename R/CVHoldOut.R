#==============================================================================#
#                               CVHoldOut                                      #
#==============================================================================#
#' CVHoldOut
#'
#' \code{CVHoldOut} Class for splitting corpus into training, validation and test sets.
#'
#' Class splits corpus into training, validation, and test sets.  Each document
#' is split according to the proportions designated in the train, val, and test
#' parameters.  The k parameter is ignored.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(train, validation, test, k)}}{Method not implemented for this abstract class.}
#' }
#'
#' @param collection File collection object to be split
#' @param train Numeric proportion of text document to be allocated to the training set
#' @param val Numeric proportion of text document to be allocated to the validation set
#' @param test Numeric proportion of text document to be allocated to the test set
#' @param k Numeric, the number of k-folds, for the k-fold cross-validation strategy
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
CVHoldOut <- R6::R6Class(
  "CVHoldOut",
  inherit = Entity,
  public = list(
    initialize = function(collection, train,  val, test, k) {



    }
  )
)

