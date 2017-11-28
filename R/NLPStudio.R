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
            if (!dir.exists(private$..path)) dir.create(private$..path, recursive = TRUE)
            c <- Constants$new()
            paths <- c$getStudioPaths()
            lapply(paths, function(p) {
              dir <- file.path(private$..path, p)
              if (!dir.exists(dir))  dir.create(dir, recursive = TRUE)
            })

            # # Create logger and initialization log entry
            private$..log <- Logger$new(file.path(private$..path, 'logs'))
            private$..log$entry$owner <- private$..name
            private$..log$entry$className <- "NLPStudio"
            private$..log$entry$methodName <- "initialize"
            private$..log$entry$path <- private$..path
            private$..log$entry$level <- "Info"
            private$..log$entry$msg <- "Initialized NLPStudio"
            private$..log$entry$fieldName <- 'nlpStudio'
            private$..log$entry$created <- Sys.time()
            private$..log$writeLog()

            invisible(self)
          },

          getInstance = function()  {
            invisible(self)
          },

          #-------------------------------------------------------------------------#
          #                           Composite Methods                             #
          #-------------------------------------------------------------------------#
          addLab = function(lab) {
            name <- lab$getName()
            private$..labs[['name']] <- lab
            invisible(self)
          },

          #-------------------------------------------------------------------------#
          #                           Visitor Method                                #
          #-------------------------------------------------------------------------#
          accept = function(visitor)  {
            visitor$nlpStudio(self)
          },

          #-------------------------------------------------------------------------#
          #                             Test Methods                                #
          #-------------------------------------------------------------------------#
          exposeObject = function() {
            o <- list(
              name <- private$..name,
              desc <- private$..desc,
              path <- private$..path,
              labs <- private$..labs,
              log <- private$..log,
              created <- private$..created,
              modified <- private$..modified
            )
            return(o)
          }

        )
      )
      super$initialize(...)
    }
  )
)#$new()
