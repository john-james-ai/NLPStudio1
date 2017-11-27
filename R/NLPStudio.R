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
          ..log = character(),
          ..created = "None",
          ..modified = "None"
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
            c <- Constants$new()
            paths <- c$getPaths()
            lapply(paths, function(p) {
              if (!dir.exists(p))  dir.create(p, recursive = TRUE)
            })

            # Create logger and initialization log entry
            private$..log <- Logger$new(private$..path)
            private$..log$entry$owner <- private$..name
            private$..log$entry$className <- "NLPStudio"
            private$..log$entry$methodName <- "initialize"
            private$..log$entry$path <- private$..path
            private$..log$entry$level <- "Info"
            private$..log$entry$msg <- "Initialized NLPStudio"
            private$..log$entry$fieldName <- 'nlpStudio'
            private$..log$entry$created <- Sys.time()
            private$..log$writeLog()

            # Assign its name in the global environment
            assign(private$..name, self, envir = .GlobalEnv)

            invisible(self)
          },

          getInstance = function()  {
            invisible(self)
          },

          #-------------------------------------------------------------------------#
          #                           Visitor Methods                               #
          #-------------------------------------------------------------------------#
          accept = function(visitor)  {
            visitor$nlpStudio(self)
          }
        )
      )
      super$initialize(...)
    }
  )
)#$new()
