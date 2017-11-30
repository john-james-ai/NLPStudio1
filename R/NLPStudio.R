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
          ..name = character(),
          ..desc = character(),
          ..path = character(),
          ..labs = list(),
          ..archives = list(),
          ..currentLab = character(),
          ..logs = character(),
          ..state = character(),
          ..created = "None",
          ..modified = "None"
        ),

        active = list(

          currentLab = function(value) {
            if (missing(value)) {
              private$..currentLab
            } else {
              name <- value$getName()
              if (!is.null(private$..labs[[name]])) {
                private$..currentLab <- value
                private$..modified <- Sys.time()
                private$..logs$entry$level <- 'Info'
                private$..logs$entry$msg <- private$..state <-
                  paste0("Current lab changed to ", name, ". ")
                private$..logs$entry$created <- Sys.time()
                private$..logs$writeLog()
              } else {
                private$..logs$entry$level <- 'Error'
                private$..logs$entry$msg <- private$..state <-
                  paste0("Unable to change current lab; ", value, ", does not exist.")
                private$..logs$entry$created <- Sys.time()
                private$..logs$writeLog()
                stop()
              }
            }
          }
        ),

        public = list(
          #-------------------------------------------------------------------#
          #                       NLPStudio Methods                           #
          #-------------------------------------------------------------------#
          initialize = function() {

            # Create single instance of NLPStudio object
            private$..name <- "nlpStudio"
            private$..desc <- "NLPStudio: Natural Language Processing Environment"
            private$..path <- "./NLPStudio"
            private$..modified <- Sys.time()
            private$..created <- Sys.time()

            # Suppress automatically generated error messages
            opt <- options(show.error.messages=FALSE, warn = -1)
            on.exit(options(opt))

            # Create Directories
            if (!dir.exists(private$..path)) dir.create(private$..path, recursive = TRUE)
            c <- Constants$new()
            paths <- c$getStudioPaths()
            lapply(paths, function(p) {
              dir <- file.path(private$..path, p)
              if (!dir.exists(dir))  dir.create(dir, recursive = TRUE)
            })

            # # Create logger and initialization log entry
            private$..logs <- Logger$new(file.path(private$..path, 'logs'))
            private$..logs$entry$owner <- private$..name
            private$..logs$entry$className <- "NLPStudio"
            private$..logs$entry$methodName <- "initialize"
            private$..logs$entry$level <- "Info"
            private$..logs$entry$msg <- private$..state <- "Initialized NLPStudio."
            private$..logs$entry$fieldName <- NA
            private$..logs$entry$created <- Sys.time()
            private$..logs$writeLog()

            # Assign its name in the global environment
            assign(private$..name, self, envir = .GlobalEnv)

            invisible(self)
          },

          getInstance = function()  {
            invisible(self)
          },

          getName = function() private$..name,

          #-------------------------------------------------------------------------#
          #                           Composite Methods                             #
          #-------------------------------------------------------------------------#
          addChild = function(child) {

            # Validation
            v <- Validator$new()
            status <- v$addChild(self$getInstance(), child)
            if (status[['code']] == FALSE) {
              private$..logs$entry$level <- "Error"
              private$..logs$entry$msg <- private$..state <- status[['msg']]
              private$..logs$entry$created <- Sys.time()
              private$..logs$writeLog()
              stop()
            }
            # Get lab name
            kidsName <- child$getName()

            # Add lab to list of labs
            private$..labs[[kidsName]] <- child

            # Set parent to nlpstudio
            child$parent <- self

            # Update modified time
            private$..modified <- Sys.time()

            # Save state and log Event
            private$..state <- private$..logs$msg <-
              paste("Lab", kidsName, "added to nlpStudio at", Sys.time())
            private$..logs$entry$created <- Sys.time()
            private$..logs$writeLog()

            # Assign its name in the global environment
            assign(private$..name, self, envir = .GlobalEnv)

            invisible(self)
          },

          removeChild = function(lab) {
            #TODO: Archive to archive folder then remove
          },

          #-------------------------------------------------------------------------#
          #                           Visitor Method                                #
          #-------------------------------------------------------------------------#
          accept = function(visitor)  {
            name <- visitor$getName()
            private$..state <- private$..logs$entry$msg <-
              paste("Accepted visitor,", name, "at", Sys.time())
            private$..logs$writeLog()
            visitor$nlpStudio(self$getInstance())
          },

          #-------------------------------------------------------------------------#
          #                             Test Methods                                #
          #-------------------------------------------------------------------------#
          exposeObject = function() {
            o <- list(
              name = private$..name,
              desc = private$..desc,
              path = private$..path,
              labs = private$..labs,
              log = private$..log,
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
