## ---- NewSingle
#==============================================================================#
#                                 NewSingletonClass                                    #
#==============================================================================#
#' NewSingletonClass
#'
#' \code{NewSingletonClass} Description goes here
#'
#' More descriptive stuff
#'
#'
#' \strong{NewSingletonClass Core Methods:}
#' \describe{
#'  \item{\code{new()}}{Initializes the NewSingletonClass as s singleton object at load time.
#' }}
#'
#'
#'
#' @docType class
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
NewSingletonClass <- R6::R6Class(
  "SingletonContainer",
  portable = FALSE,
  inherit = Singleton,
  public = list(
    initialize = function(...) {
      Class <<- R6::R6Class(
        classname = "NewSingletonClass",
        private = list(
          ..name = character(0),
          ..desc = character(0),
          ..stateId = character(0),
          ..stateDesc = character(0),
          ..created = "None",
          ..modified = "None"
        ),

        public = list(
          #-------------------------------------------------------------------#
          #                       NewSingletonClass Methods                   #
          #-------------------------------------------------------------------#
          initialize = function() {

            #TODO: Add some code

            # Assign its name in the global environment
            assign(name, self, envir = .GlobalEnv)

            invisible(self)
          },

          getInstance = function()  {
            invisible(self)
          },

          #-------------------------------------------------------------------------#
          #                           Visitor Methods                               #
          #-------------------------------------------------------------------------#
          accept = function(visitor)  {
            visitor$NewSingletonClass(self)
          }
        )
      )
      super$initialize(...)
    }
  )
)#$new()
