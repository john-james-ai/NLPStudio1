#==============================================================================#
#                                 NLPSuper                                     #
#==============================================================================#
#' NLPSuper
#'
#' \code{NLPSuper} This is a super class that contains all common methods to
#' be implemented by all classes.
#'
#' This class defines the common methods, such as logging and error handling
#' required by all classes
#'
#' @section Class methods:
#'
#' \strong{NLPSuper Core Methods:}
#'  \itemize{
#'   \item{\code{new(name, desc)}}{Instantiates class. Not implemented for this class.}
#'   \item{\code{logger(level, msg, fieldName)}}{Formats a log entry and passes it to the Logger object. }
#' }
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
NLPSuper <- R6::R6Class(
  classname = "NLPSuper",
  lock_objects = TRUE,
  lock_class = TRUE,

  private = list(
    logger = function(level, msg, fieldName) {
      entry <- LogEntry$new()
      entry$class <- class(self)
      entry$method <- match.call()[1]
      entry$level <- level
      entry$msg <- msg
      entry$fieldName <- ifelse(is.null(fieldName), "", fieldName)
      log <- Logger$new()
      log$write(entry)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#
    init = function() {stop("This method is not implemented for the NLPSuper Class. ")}

  )
)
