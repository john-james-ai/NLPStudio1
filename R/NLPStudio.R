## ---- NLPStudio
#==============================================================================#
#                                 NLPStudio                                    #
#==============================================================================#
#' NLPStudio
#'
#' \code{NLPStudio} Class the creates and manages labs.
#'
#' This class creates and manages data labs  A data lab is essentially a
#' directory in which project data reside.  Multiple data labs can be created
#' to house separate versions of the data for analysis. Note: This class is a
#' singleton pattern. An NLPStudio object called nlpStudio is instantiated at
#' load time.  Any subsequent initializations will return the single nlpStudio
#' instance. There are two sets of methods.  The first set enables clients to
#' retrieve information about the NLPStudio object.  The second set allows
#' clients to add, remove, enter, and leave labs.
#'
#' \strong{NLPStudio Core Methods:}
#' \describe{
#'  \item{\code{new()}}{Initializes the NLPStudio as s singleton object at load time.}
#'
#'  \item{\code{getInstance()}}{Returns the current NLPStudio instance object. This will be the only instantiation called "nlpStudio.},
#' }
#'
#'
#'
#' @docType class
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
NLPStudio <- R6::R6Class(
  "SingletonContainer",
  portable = FALSE,
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Singleton,
  public = list(
    initialize = function(...) {
      Class <<- R6::R6Class(
        classname = "NLPStudio",
        private = list(
          ..className = 'NLPStudio',
          ..methodName = character(),
          ..name = character(),
          ..desc = character(),
          ..path = character(),
          ..labs = list(),
          ..logs = character(),
          ..state = character(),
          ..created = "None",
          ..modified = "None"
        ),

        public = list(
          #-------------------------------------------------------------------#
          #                       NLPStudio Methods                           #
          #-------------------------------------------------------------------#
          initialize = function() {

            # Create single instance of NLPStudio object
            private$..className <- 'NLPStudio'
            private$..methodName <- 'initialize'
            private$..name <- "nlpStudio"
            private$..desc <- "NLPStudio: Natural Language Processing Environment"
            private$..path <- "./NLPStudio"
            private$..modified <- Sys.time()
            private$..created <- Sys.time()

            # Create NLPStudio home directory
            if (!dir.exists(private$..path)) dir.create(private$..path, recursive = TRUE)

            # # Create logger and initialization log entry
            private$..logs <- LogR$new(file.path(private$..path, 'logs'))
            private$..state <- paste0("Initialized NLPStudio.")
            self$logIt()

            # Assign its name in the global environment
            assign(private$..name, self, envir = .GlobalEnv)
            invisible(self)
          },

          getInstance = function()  {
            invisible(self)
          },

          getName = function() private$..name,
          getPath = function() private$..path,
          getClassName = function() private$..className,

          #-------------------------------------------------------------------------#
          #                           Composite Methods                             #
          #-------------------------------------------------------------------------#

          getLabs = function() private$..labs,

          addLab = function(lab) {

            # Update current method
            private$..methodName <- 'addLab'

            # Validation
            v <- Validator$new()
            status <- v$addChild(self, lab)
            if (status[['code']] == FALSE) {
              private$..state <- status[['msg']]
              self$logIt(level = 'Error')
              stop()
            }
            # Get lab name
            labName <- lab$getName()

            # Add lab to list of labs
            private$..labs[[labName]] <- lab

            # Set parent to nlpstudio
            lab$parent <- self

            # Update modified time
            private$..modified <- Sys.time()

            # Save state and log Event
            private$..state <-
              paste("Lab", labName, "added to nlpStudio at", Sys.time())
            self$logIt()

            # Assign its name in the global environment
            assign(private$..name, self, envir = .GlobalEnv)
            invisible(self)

          },

          removeLab = function(lab) {
            #TODO: Archive to archive folder then remove
            #TODO: Don't allow current lab to be removed.

            # Update current method
            private$..methodName <- 'removeLab'

            # Validation
            v <- Validator$new()
            status <- v$removeChild(self, document)
            if (status[['code']] == FALSE) {
              private$..state <- status[['msg']]
              self$logIt(level = 'Error')
              stop()
            }

            name <- lab$getName()
            if (!is.null(private$..labs[[name]]))  private..labs[[name]] <- NULL

            # Assign its name in the global environment
            assign(private$..name, self, envir = .GlobalEnv)
            invisible(self)

          },

          #-------------------------------------------------------------------------#
          #                           Visitor Method                                #
          #-------------------------------------------------------------------------#
          accept = function(visitor)  {
            visitor$nlpStudio(self)
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
          #                             Test Methods                                #
          #-------------------------------------------------------------------------#
          exposeObject = function() {
            o <- list(
              className = private$..className,
              methodName = private$..methodName,
              name = private$..name,
              desc = private$..desc,
              path = private$..path,
              labs = private$..labs,
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
