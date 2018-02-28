#==============================================================================#
#                                   Adaptor0                                   #
#==============================================================================#
#' Adaptor0
#'
#' \code{Adaptor0} Abstract class for Adaptor classes. 
#' 
#' Defines interface for concrete Adaptor classes.
#' 
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Adaptor family of classes
#' @export
Adaptor0 <- R6::R6Class(
  classname = "Adaptor0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,
  
  private = list(
    ..x = character(),
    ..format = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function(x, format) { stop("This method is not implemented for this abstract class.") },
    #-------------------------------------------------------------------------#
    #                              Adapt Method                               #
    #-------------------------------------------------------------------------#
    adapt = function() { 
      
      if (sum(c("Corpus", "Entity") %in% class(private$..x)) == 2) {
        return(private$adaptTo())
      } else {
        return(private$adaptFrom())
      }
    },
    #-------------------------------------------------------------------------#
    #                           Validation Methods                            #
    #-------------------------------------------------------------------------#
    getParams = function() {
      params <- list()
      params$x <- private$..x
      params$format <- private$..format
      return(params)
    }
  )
)
