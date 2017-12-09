## ---- Command0
#==============================================================================#
#                               Command0                                 #
#==============================================================================#
#' Command0
#'
#'
#' \code{Command0} Abstract class for the command classes.
#'
#' Abstract class for the command classes.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{execute()}}{Method that executes the command passed in from the Parser Class }
#' }
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Command0 <- R6::R6Class(
  "Command0",
  inherit = Entity,
  public = list(
    execute = function() stop("The method is not implemented for this abstract class.")
  )
)

