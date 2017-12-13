#==============================================================================#
#                                    VIO                                       #
#==============================================================================#
#' VIO
#'
#' \code{Data} Visitor class reasonsible for reading and writing documents of various formats.
#'
#'
#' @section VIO methods:
#' \describe{
#'  \item{\code{new()}}{Instantiates a VIO object for Document object.}
#'  \item{\code{read(document)}}{Dispatches the read visitor, via the accept method of object.}
#'  \item{\code{write(document)}}{Dispatches the write visitor, via the accept method of object.}
#' }
#'
#' @param document Document family object
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family VIO family of classes
#' @export
VIO <- R6::R6Class(
  "VIO",
  lock_class = FALSE,
  lock_objects = FALSE,

  public = list(

    #-------------------------------------------------------------------------#
    #                              IO Methods                                 #
    #-------------------------------------------------------------------------#
    read = function(document) {
      visitor <- VIORead$new()
      document$accept(visitor)
    },

    write = function(document) {
      visitor <- VIOWrite$new()
      document$accept(visitor)
    }
  )
)
