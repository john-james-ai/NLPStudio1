#==============================================================================#
#                           CorpusImport0                                      #
#==============================================================================#
#' CorpusImport0
#'
#' \code{CorpusImport0} Abstract class for the corpus Import family of classes.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(x)}}{Instantiates an object of the Import family of classes.}
#'  \item{\code{execute()}}{Executes the corpus import.}
#' }
#'
#' @param x The corpus source object. Supported formats include:
#' \itemize{
#'  \item Character Vectors
#'  \item Lists containing character vectors
#'  \item Data frames
#'  \item Quanteda corpus objects
#'  \item TM corpus objects
#'  \item koRpus package objects
#' }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusImport classes
#' @export
CorpusImport0 <- R6::R6Class(
  "CorpusImport0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..source = character()
  ),


  public = list(
    initialize = function(x) stop("The method is not implemented for this abstract class."),
    execute = function(x) stop("The method is not implemented for this abstract class.")
  )
)
