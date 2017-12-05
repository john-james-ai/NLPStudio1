## ---- VReadState
#==============================================================================#
#                                   VReadState                              #
#==============================================================================#
#' VReadState
#'
#'
#' \code{VReadState} Visitor class responsible for restoring objects to prior states
#'
#' \strong{VReadState Methods:}
#' The VReadState methods are as follows:
#'  \itemize{
#'   \item{\code{lab(object)}}{Method for saving the current state of Lab objects.}
#'   \item{\code{documentCollection(object)}}{Method for saving the current state of DocumentCollection objects.}
#'   \item{\code{document(object)}}{Method for saving the current state of Document objects.}
#' }
#'
#' @param object Object to be saved
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @export
VReadState <- R6::R6Class(
  classname = "VReadState",
  private = list(

    validateObject = function(object) {

      constants <- Constants$new()

      v <- ValidatorClass$new()
      if (v$validate(class = "VReadState", method = method, fieldName = "class(object)",
                     level = "Error", value = class(object)[1],
                     msg = paste("Unable to restore object.",
                                 "Object is not of a serializable class.",
                                 "See ?VReadState for assistance."),
                     expect = constants$getStateClasses()) == FALSE) {
        stop()
      }
    }
  ),
  public = list(

    lab = function()  {
      private$..validateObject(object)

      private$..reinstate
    },
    documentCollection = function()  {
      private$..validateObject(object)
      state <- stateManager$restoreState(object)
    },
    document = function()  {
      private$..validateObject(object)
      state <- stateManager$restoreState(object)
    }
  )
)
