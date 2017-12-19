#==============================================================================#
#                                     CV0                                      #
#==============================================================================#
#' CV0
#'
#' \code{CV0} Abstract class for the classes that implement the cross validation data split functionality
#'
#' Abstract class for splitting the text data according to several
#' cross-validation strategies.  At present, the hold-out and k-fold strategies
#' are supported.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(train, validation, test, k)}}{Method not implemented for this abstract class.}
#' }
#'
#' @param train Numeric proportion of text document to be allocated to the training set
#' @param val Numeric proportion of text document to be allocated to the validation set
#' @param test Numeric proportion of text document to be allocated to the test set
#' @param k Numeric, the number of k-folds, for the k-fold cross-validation strategy
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
CV0 <- R6::R6Class(
  "CV0",
  inherit = Entity,
  public = list(
    initialize = function(train, validation, test, k) stop("The method is not implemented for this abstract class.")
  )
)

