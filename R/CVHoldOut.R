#==============================================================================#
#                                CVHoldOut                                     #
#==============================================================================#
#' CVHoldOut
#'
#' \code{CVHoldOut} Class responsible for creating the cross-validation hold out sets
#'
#' \strong{CV Class Overview:}
#' The CVHoldOut class is an implementation of the strategy design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This strategy pattern accommodates
#' various formats and allows the behavior to be defined / selected at run time.
#'
#' @section CVHoldOut Methods:
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
CVHoldOut <- R6::R6Class(
  classname = "CVHoldOut",
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

      private$..className <- 'CVHoldOut'
      private$..methodName <- 'initialize'
      private$..fc <- fc
      private$..pTrain = pTrain
      private$..pVal = pVal
      private$..pTest = pTest
      private$..k <- k
      private$..stratified <- stratified
      private$..created <- Sys.time()
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()
      private$..state <- paste0("Initialized CVHoldOut Class")
      self$logIt()

      invisible(self)

    },

    execute = function() {

      private..admin$methodName <- 'execute'

      files <- private$..fc$getFiles()

      if (private$..stratified == TRUE) {

        subsets <- lapply(files, function(f) {
          content <- f$read()
          set.seed(73)
          content <- sample(content)
          s1 <- floor(length(content) * private$..pTrain)
          s2 <- floor(length(content) * (private$..pTrain + private$..pVal))
          cvSet <- list()
          cvSet[['train']] <- content[1:s1]
          cvSet[['val']] <- content[(s1+1):s2]
          cvSet[['test']] <- content[(s2+1):length(content)]
          cvSet
        })

        private$..cv[['train']] <-  unlist(lapply(subsets, function(s) {
          s[['train']]
        }))
        private$..cv[['val']]  <- unlist(lapply(subsets, function(s) {
          s[['val']]
        }))
        private$..cv[['test']]  <- unlist(lapply(subsets, function(s) {
          s[['test']]
        }))
      } else {

        content <- unlist(fc$read())
        set.seed(732)
        content <- sample(content)
        s1 <- floor(length(content) * private$..pTrain)
        s2 <- floor(length(content) * (private$..pTrain + private$..pVal))
        private$..cv[['train']] <- content[1:s1]
        private$..cv[['val']] <- content[(s1+1):s2]
        private$..cv[['test']] <- content[(s2+1):length(content)]
      }

      private$..state <- paste0("Created Hold Out Cross Validation Sets")
      self$logIt()

      return(private$..cv)
    }
  )
)
