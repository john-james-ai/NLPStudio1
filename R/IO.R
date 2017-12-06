## ---- IO
#==============================================================================#
#                                      IO                                      #
#==============================================================================#
#' IO
#'
#'
#' \code{IO} Class responsible for reading and writing xlsx, csv, text, and rdata files through the read and write visitor methods.
#'
#' \strong{IO Class Overview:}
#' The IO class is an implementation of the visitor design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This visitor pattern allows
#' new operations to be defined without changing the classes upon which
#' the visitor method operates.
#'
#' \strong{IO Methods:}
#' The IO class supports csv, Rdata, and text files through the following methods:
#'  \itemize{
#'   \item{\code{read(document)}}{Read method.}
#'   \item{\code{write(document)}}{Write method.}
#' }
#'
#' @param document Object of the Document family of classes
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Read/Write Classes
#' @export
IO <- R6::R6Class(
  classname = "IO",
  lock_objects = TRUE,
  lock_class = FALSE,
  private = list(),
  public = list(

    read = function(document) {
      visitor <- VReader$new()
      document$accept(visitor)
    },
    write = function(document) {
      visitor <- VWriter$new()
      document$accept(visitor)
    }
  )
)
