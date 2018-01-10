#==============================================================================#
#                                   IOFactory                                  #
#==============================================================================#
#' IOFactory
#'
#' \code{IOFactory} Factory class that creates the appropriate IO class.
#'
#' Class creates IO objects based upon the file type
#'
#' @section IOFactory methods:
#' \describe{
#'  \item{\code{new()}}{Instantiates the factory.}
#'  \item{\code{getFileStrategy(fileName)}}{Instantiates and returns the appropriate File Strategy object.}
#' }
#'
#' @section Validation Class Parameters
#' @param fileName Character string containing the name of the file
#'
#' @return File class object
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Validation Classes
#' @export
IOFactory <- R6::R6Class(
  "IOFactory",
  lock_class = FALSE,
  lock_objects = FALSE,

  private = list(
    ..path = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                      Object Creation and Read                           #
    #-------------------------------------------------------------------------#
    initialize = function(path) {
      private$..path <- path
      invisible(self)
    },

    getIOStrategy = function() {

      type <- tolower(tools::file_ext(private$..path))

      io <- switch(type,
                   txt = IOText$new(),
                   csv = IOCSV$new(),
                   rdata = IORdata$new(),
                   rds = IORDS$new(),
                   NULL)

      return(io)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$IOFactory(self)
    }
  )
)
