#==============================================================================#
#                                CVKFold                                       #
#==============================================================================#
#' CVKFold
#'
#' \code{CVKFold} Class responsible for creating the K-fold cross-validation sets
#'
#' \strong{CV Class Overview:}
#' The CVKFold class is an implementation of the strategy design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This strategy pattern accommodates
#' various formats and allows the behavior to be defined / selected at run time.
#'
#' @section CVKFold Methods:
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
#' @param stratified Boolean indicating whether sampling should be stratified.
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Cross-Validation Classes
#' @export
CVKFold <- R6::R6Class(
  classname = "CVKFold",
  lock_objects = TRUE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..fc = character(),
    ..k = integer(),
    ..pTrain = numeric(),
    ..pVal = numeric(),
    ..pTest = numeric(),
    ..stratified = TRUE,
    ..cv = list()
  ),

  public = list(

    initialize = function(fc, pTrain = 0.6, pVal = 0.2, pTest = 0.2, k = 10, stratified = TRUE) {

      private$..className <- 'CVKFold'
      private$..methodName <- 'initialize'
      private$..fc <- fc
      private$..pTrain = pTrain
      private$..pVal = pVal
      private$..pTest = pTest
      private$..k <- k
      private$..stratified <- stratified
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()
      private$..state <- paste0("Initialized CVKFold Class")
      self$logIt()

      invisible(self)

    },

    execute = function() {

      private..admin$methodName <- 'execute'

      content <- unlist(private$..fc$read())
      set.seed(73)
      content <- sample(content)
      foldSize <- floor(length(content) / private$..k)
      folds <- list()
      for (i in 1:private$..k) {
        folds[[i]] <- content[1:foldSize]
        content <- content[(foldSize + 1):(length(content))]
      }

      private$..cv <- lapply(seq_along(folds), function(f) {
        cvSet <- list()
        cvSet[['train']] <- unlist(folds[-f])
        cvSet[['val']] <- unlist(folds[f])
        cvSet
      })

      private$..state <- paste0("Created K-Fold Cross-Validation Sets")
      self$logIt()

      return(private$..cv)
    }
  )
)
