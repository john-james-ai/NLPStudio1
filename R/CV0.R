#==============================================================================#
#                                      CV0                                     #
#==============================================================================#
#' CV0
#'
#'
#' \code{CV0} Abstract class for the cross validation family of classes.
#'
#' \strong{CV Class Overview:}
#' The CV0 class is an implementation of the strategy design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This strategy pattern accommodates
#' various formats and allows the behavior to be defined / selected at run time.
#'
#' @section CV0 Methods:
#'  \describe{
#'   \item{\code{new(fc, pTrain, pVal, pTest, k = NULL)}}{Not implemented for this abstract class.}
#'   \item{\code{execute()}}{Not implemented for this abstract class.}
#' }
#'
#' @param fc FileCollection object from which the cross-validation sets will be created.
#' @param k Integer indicating the number of folds for the K-FOLD cross-validation class.
#' @param pTrain Numeric between 0 and 1, representating the proportion of the file to be allocated for the training set
#' @param pVal Numeric between 0 and 1, representating the proportion of the file to be allocated for the validation set
#' @param pTest Numeric between 0 and 1, representating the proportion of the file to be allocated for the test set
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Cross-Validation Classes
#' @export
CV0 <- R6::R6Class(
  classname = "CV0",
  lock_objects = TRUE,
  lock_class = FALSE,
  inherit = Entity,
  public = list(

    initialize = function(fc, pTrain, pVal, pTest, k = NULL) stop("This method is not implemented for this abstract class."),
    execute = function() stop("This method is not implemented for this abstract class.")
  )
)
