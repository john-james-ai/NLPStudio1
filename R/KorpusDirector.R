#==============================================================================#
#                             KorpusDirector                                   #
#==============================================================================#
#' KorpusDirector
#'
#' \code{KorpusDirector} Class that directs the building of Korpus objects
#'
#' The Document family of classes is an implementation of the builder
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows the
#' construction process to be defined at runtime.
#'
#' @section Korpus Director Methods:
#'  \describe{
#'   \item{\code{new(name, desc = NULL, lab = NULL)}}{Creates an object of Korpus Class}
#'   \item{\code{desc}}{A getter/setter method allowing clients to retrieve and set the Korpus description variable.}
#'   \item{\code{lab}}{A getter/setter method allowing clients to retrieve and set the Lab object to which the Korpus object belongs.}
#'   \item{\code{getName()}}{Returns the name of the Korpus object.}
#'   \item{\code{getPath()}}{Returns the path of the Korpus object.}
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#'  }
#'
#'   \item{\code{splitCorpus()}}{Method for initiating the repair operation on a document.}
#' }
#'
#' @param builder Corpus builder object
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus build family
#' @export
KorpusDirector <- R6::R6Class(
  classname = "KorpusDirector",
  lock_objects = FALSE,
  lock_class = FALSE,
  private = list(
    ..className = 'KorpusDirector',
    ..methodName = character(),
    ..builder = character(),
    ..state = character(),

    ..logs = character(),
    ..modified = "None",
    ..created = "None"
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Korpus Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(builder) {

      # Instantiate variables
      private$..className <- 'KorpusDirector'
      private$..methodName <- 'initialize'
      private$..name <- 'korpusDirector'
      private$..state <- "Instantiated the corpus build director."
      private$..builder <- builder

      # Validate Korpus
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Log it
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                          Construct Methods                              #
    #-------------------------------------------------------------------------#
    construct = function() {


    },


    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$korpus(self)
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
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      director = list(
        className = private$..className,
        methodName = private$..methodName,
        name = private$..name,
        builder = private$..builder,
        state = private$..state,
        modified = private$..modified,
        created = private$..created
      )

      return(director)
    }
  )
)
