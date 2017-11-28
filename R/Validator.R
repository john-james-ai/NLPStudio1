#==============================================================================#
#                                 Validator                                    #
#==============================================================================#
#' Validator
#'
#' @description
#' \code{Validator} Class responsible for validation of requests pertaining to:
#' \itemize{
#'  \item Object Instantiation
#'  \item Composition and Aggregation: Requests to manipulate aggregate and composite objects
#'  \item Document I/O: Requests to read and write documents.
#' }
#'
#' @section Validator methods:
#' This section summarizes the methods in the Validator class.
#'
#' \strong{Object Create and Read Methods:}
#' \describe{
#'  \item{\code{new()}}{Creates an object of Validator class}
#'  \item{\code{init(object)}}{Dispatches the initialization validation visitor, via the accept method of object.}
#'  \item{\code{exposeObject(object)}}{Dispatches the exposeObject validation visitor, via the accept method of object.}
#'  }
#'
#' \strong{Composition and Aggregation Methods:}
#' \describe{
#'  \item{\code{addChild(object)}}{Dispatches the addChild validation visitor, via the accept method of object.}
#'  \item{\code{removeChild(object)}}{Dispatches the removeChild validation visitor, via the accept method of object.}
#'  \item{\code{setParent(object, parent)}}{Dispatches the setParent validation visitor, via the accept method of object.}
#'  }
#'
#' \strong{Curator Methods:}
#' \describe{
#'  \item{\code{setQueryTarget(object, target)}}{Dispatches the setQueryTarget validation visitor, via the accept method of object.}
#'  \item{\code{submitQuery(object)}}{Dispatches the removeChild validation visitor, via the accept method of object.}
#'  }
#'
#' \strong{History Methods:}
#' \describe{
#'  \item{\code{readHistory(object)}}{Dispatches the readHistory validation visitor, via the accept method of object.}
#'  }
#'
#' \strong{State Methods:}
#' \describe{
#'  \item{\code{readStates(object)}}{Dispatches the readStates validation visitor, via the accept method of object.}
#'  \item{\code{saveState(object)}}{Dispatches the saveState validation visitor, via the accept method of object.}
#'  \item{\code{restoreState(object)}}{Dispatches the restoreState validation visitor, via the accept method of object.}
#'  }
#'
#' \strong{Document I/O Methods:}
#' \describe{
#'  \item{\code{read(object)}}{Dispatches the read validation visitor, via the accept method of object.}
#'  \item{\code{write(object)}}{Dispatches the write validation visitor, via the accept method of object.}
#' }
#'
#' @section Validation Class Parameters
#' @param object Originator object
#' @param target Object which is the target of a query. Used by the Curator methods
#' @param content Character vector containing content to be written by a write originator.
#'
#' @return A logical, TRUE if validation criteria are met, FALSE, otherwise.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Validation Classes
#' @export
Validator <- R6::R6Class(
  "Validator",
  lock_class = FALSE,
  lock_objects = FALSE,

  public = list(

     )
)
