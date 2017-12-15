## ---- IO0
#==============================================================================#
#                                      IO0                                      #
#==============================================================================#
#' IO0
#'
#'
#' \code{IO0} Abstract class that defines the interface for reading and writing documents.
#'
#' \strong{IO Class Overview:}
#' The IO0 class is an implementation of the strategy design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This strategy pattern accommodates
#' various formats and allows the behavior to be defined / selected at run time.
#'
#' \strong{IO0 Methods:}
#' The IO0 class supports csv, Rdata, and text files through the following methods:
#'  \itemize{
#'   \item{\code{read(document)}}{Read method.}
#'   \item{\code{write(document)}}{Write method.}
#' }
#'
#' @param document Object of the Document family of classes
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Input / Output Classes
#' @export
IO0 <- R6::R6Class(
  classname = "IO0",
  lock_objects = TRUE,
  lock_class = FALSE,
  private = list(),
  public = list(

    read = function(document) stop("This method is not implemented for this abstract class."),
    write = function(document) stop("This method is not implemented for this abstract class.")
  )
)
