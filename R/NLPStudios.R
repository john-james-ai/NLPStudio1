## ---- NLPStudios
#==============================================================================#
#                                 NLPStudios                                    #
#==============================================================================#
#' NLPStudios
#'
#' \code{NLPStudios} Class the creates and manages studios.
#'
#' This class creates and manages data studios  A data studio is essentially a
#' directory in which project data reside.  Multiple data studios can be created
#' to house separate versions of the data for analysis. Note: This class is a
#' singleton pattern. An NLPStudios object called nlpStudios is instantiated at
#' load time.  Any subsequent initializations will return the single nlpStudios
#' instance. There are two sets of methods.  The first set enables clients to
#' retrieve information about the NLPStudios object.  The second set allows
#' clients to add, remove, enter, and leave studios.
#'
#' \strong{NLPStudios Core Methods:}
#' \describe{
#'  \item{\code{new()}}{Initializes the NLPStudios as s singleton object at load time.}
#'
#'  \item{\code{getInstance()}}{Returns the current NLPStudios instance object. This will be the only instantiation called "nlpStudios.},
#' }
#'
#'
#'
#' @docType class
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
NLPStudios <- R6::R6Class(
  "SingletonContainer",
  portable = FALSE,
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Singleton,
  public = list(
    initialize = function(...) {
      Class <<- R6::R6Class(
        classname = "NLPStudios",
        private = list(
          ..className = 'NLPStudios',
          ..methodName = character(),
          ..name = character(),
          ..desc = character(),
          ..path = character(),
          ..studios = list(),
          ..logs = character(),
          ..state = character(),
          ..created = "None",
          ..modified = "None"
        ),

        public = list(
          #-------------------------------------------------------------------#
          #                       NLPStudios Methods                           #
          #-------------------------------------------------------------------#
          initialize = function() {

            # Create single instance of NLPStudios object
            private$..className <- 'NLPStudios'
            private$..methodName <- 'initialize'
            private$..name <- "nlpStudios"
            private$..desc <- "NLPStudios: Natural Language Processing Environment"
            private$..path <- "./NLPStudios"
            private$..modified <- Sys.time()
            private$..created <- Sys.time()

            # Create NLPStudios home directory
            if (!dir.exists(private$..path)) dir.create(private$..path, recursive = TRUE)

            # # Create logger and initialization log entry
            private$..logs <- LogR$new()
            private$..state <- paste0("Initialized NLPStudios.")
            self$logIt()

            # Assign its name in the global environment
            assign(private$..name, self, envir = .GlobalEnv)
            invisible(self)
          },

          getInstance = function()  {
            invisible(self)
          },

          #-------------------------------------------------------------------------#
          #                             Basic Getters                               #
          #-------------------------------------------------------------------------#
          getName = function() private$..name,
          getClassName = function() private$..className,
          getPath = function() private$..path,

          #-------------------------------------------------------------------------#
          #                           Composite Methods                             #
          #-------------------------------------------------------------------------#

          getStudios = function() private$..studios,

          addStudio = function(studio) {

            # Update current method
            private$..methodName <- 'addStudio'

            # Validation
            v <- Validator$new()
            status <- v$addChild(self, studio)
            if (status[['code']] == FALSE) {
              private$..state <- status[['msg']]
              self$logIt(level = 'Error')
              stop()
            }
            # Get studio name
            studioName <- studio$getName()

            # Add studio to list of studios
            private$..studios[[studioName]] <- studio

            # Set parent to nlpstudios
            studio$parent <- self

            # Update modified time
            private$..modified <- Sys.time()

            # Save state and log Event
            private$..state <-
              paste("Studio", studioName, "added to nlpStudios at", Sys.time())
            self$logIt()

            # Assign its name in the global environment
            assign(private$..name, self, envir = .GlobalEnv)
            invisible(self)

          },

          removeStudio = function(studio) {
            #TODO: Archive to archive folder then remove
            #TODO: Don't allow current studio to be removed.

            # Update current method
            private$..methodName <- 'removeStudio'

            # Validation
            v <- Validator$new()
            status <- v$removeChild(self, document)
            if (status[['code']] == FALSE) {
              private$..state <- status[['msg']]
              self$logIt(level = 'Error')
              stop()
            }

            name <- studio$getName()
            if (!is.null(private$..studios[[name]]))  private..studios[[name]] <- NULL

            # Assign its name in the global environment
            assign(private$..name, self, envir = .GlobalEnv)
            invisible(self)

          },

          #-------------------------------------------------------------------------#
          #                            Log Method                                   #
          #-------------------------------------------------------------------------#
          logIt = function(level = 'Info', fieldName = NA) {

            private$..logs$entry$owner <- private$..name
            private$..logs$entry$className <- private$..className
            private$..logs$entry$methodName <- private$..methodName
            private$..logs$entry$level <- level
            private$..logs$entry$msg <- private$..state
            private$..logs$entry$fieldName <- fieldName
            private$..logs$created <- Sys.time()
            private$..logs$writeLog()
          },

          #-------------------------------------------------------------------------#
          #                           Visitor Method                                #
          #-------------------------------------------------------------------------#
          accept = function(visitor)  {
            visitor$nlpStudios(self)
          },

          #-------------------------------------------------------------------------#
          #                             Test Methods                                #
          #-------------------------------------------------------------------------#
          exposeObject = function() {
            o <- list(
              className = private$..className,
              methodName = private$..methodName,
              name = private$..name,
              desc = private$..desc,
              path = private$..path,
              studios = private$..studios,
              logs = private$..logs,
              state = private$..state,
              created = private$..created,
              modified = private$..modified
            )
            return(o)
          }

        )
      )
      super$initialize(...)
    }
  )
)#$new()
