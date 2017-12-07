#==============================================================================#
#                             CorpusDirector                                   #
#==============================================================================#
#' CorpusDirector
#'
#' \code{CorpusDirector} Class that directs the building of Corpus objects
#'
#' The Document family of classes is an implementation of the builder
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows the
#' construction process to be defined at runtime.
#'
#' @section Corpus Director Methods:
#'  \describe{
#'   \item{\code{new(builder)}}{Creates an object of Corpus Class}
#'   }
#'
#' @param builder Corpus builder object
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus build family
#' @export
CorpusDirector <- R6::R6Class(
  classname = "CorpusDirector",
  lock_objects = FALSE,
  lock_class = FALSE,
  private = list(
    ..className = 'CorpusDirector',
    ..methodName = character(),
    ..builder = character(),
    ..state = character(),
    ..logs = character(),
    ..modified = "None",
    ..created = "None"
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Corpus Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(builder) {

      # Instantiate variables
      private$..className <- 'CorpusDirector'
      private$..methodName <- 'initialize'
      private$..name <- 'corpusDirector'
      private$..state <- "Instantiated the corpus build director."
      private$..builder <- builder
      private$..logs <- NLPStudio$new()$getInstance()$getDirs()$logs

      # Validate Corpus
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
    constructCorpus = function() {



    },


    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpus(self)
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
