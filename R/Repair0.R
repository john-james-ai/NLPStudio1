#==============================================================================#
#                                 Repair0                                      #
#==============================================================================#
#' Repair0
#'
#' \code{Repair0} Abstract class for the Repair family of classes.
#'
#' This abstract class defines the methods and interfaces common to all
#' Repair family classes.
#'
#' @template repairClasses.R
#' @template repairMethods.R
#' @template repairParams.R
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Repair Family of Classes
#' @export
Repair0 <- R6::R6Class(
  classname = "Repair0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..in = character(),
    ..out = character(),
    ..name = character(),
    ..patterns = data.frame(dec = c(0,1,2,3,4,5,6,7,8,9,
                                10,11,12,13,14,15,16,17,18,19,
                                20,21,22,23,24,25,26,27,28,29,30,31),
                        id = c("NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                               "BS","TAB","LF","VT","FF","CR","SO","SI","DLE",
                               "DC1","DC2","DC3","DC4","NAK","SYN","ETB","CAN",
                               "EM","SUB","ESC","FS","GS","RS","US"),
                        flag = c(TRUE,FALSE,FALSE,FALSE,FALSE,
                                 FALSE,FALSE,FALSE,FALSE,FALSE,
                                 FALSE,FALSE,FALSE,FALSE,FALSE,
                                 FALSE,FALSE,FALSE,FALSE,FALSE,
                                 FALSE,FALSE,FALSE,FALSE,FALSE,
                                 FALSE,TRUE,FALSE,FALSE,FALSE,
                                 FALSE,FALSE)),

    setPatterns = function(patterns) {
      if (!is.null(patterns)) {
        private$..patterns[["flag"]] <- FALSE
        if (class(patterns) == "numeric") {
            private$..patterns$flag[private$..patterns$dec %in% patterns] <- TRUE
        } else {
          mismatches <- which(pattern != private$..patterns$id)
          if (length(mismatches) > 0) {
            private$..state <- paste0("Invalid pattern variables: ", mismatches,
                                      " are invalid. Valid values include ",
                                      private$..patterns$id, ".")
            self$logIt("Error")
            stop()
          } else {
            private$..patterns$flag[private$..patterns$id %in% patterns] <- TRUE
          }
        }
      }
    }
  ),

  public = list(

    initialize = function(object, name, patterns = NULL) { stop("This method is not implemented for this abstract class") },
    repair = function() { stop("This method is not implemented for this abstract class") },
    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$Repair0(self)
    }
  )
)
