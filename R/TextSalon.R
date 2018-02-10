#==============================================================================#
#                               TextSalon                                      #
#==============================================================================#
#' TextSalon
#'
#' \code{TextSalon} Class for performing text cleaning and preprocessing
#'
#' @template textSalonClasses
#'
#' @section TextSalon methods:
#' \strong{Core Methods:}
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a TextSalon.}
#'   \item{\code{addCommand()}}{Method that adds a text processing command to the queue. }
#'   \item{\code{removeCommand()}}{Method that for removing a command from the queue.}
#'   \item{\code{execute()}}{Method that executes the job queue. }
#'   \item{\code{getResult()}}{Method that returns the object following execution of the job queue. }
#'  }
#'
#' \strong{Other Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object. Not implemented for this abstract class}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the TextSalon.}
#'  }
#'
#' @section Parameters:
#' @param object The object to be processed.
#' @param queue The job queue containing text processing commands.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family TextSalon classes
#' @export
TextSalon <- R6::R6Class(
  classname = "TextSalon",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..x = character(),
    ..jobQueue = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x) {

      # Instantiate variables
      private$..className <- 'TextSalon'
      private$..methodName <- 'initialize'
      private$..state <- paste0("TextSalon, ", private$..meta[["name"]], ", instantiated.")
      private$..logs <- LogR$new()
      private$..created <- Sys.time()
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()

      # Validation
      if (!class(x)[1] %in% c("Document", "Corpus")) {
        private$..state <- paste0("Invalid object. Object must be of the ",
                                  "Document or Corpus class.  See ?", class(self)[1],
                                  " for further assistance.")
        self$logIt("Error")
        stop()
      }

      # Load object
      private$..x <- object

      # Create log entry
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Command Management                            #
    #-------------------------------------------------------------------------#
    addCommand = function(cmd) {

      private$..methodName <- "addCommand"

      if (!c("TextCommand0") %in% class(cmd)) {
        private$..state <- paste0("Invalid text command object. Object must be ",
                                  "of the TextCmd classes.  See ?", class(self)[1],
                                  " for further assistance.")
        self$logIt("Error")
        stop()
      }

      name <- cmd$getName()
      private$..jobQueue[[name]] <- cmd

      private$..state <- paste0("Added ", cmd$getName(), " to ", private$..x$getName(),
                                " job queue." )
      self$logIt()

      invisible(self)
    },

    removeCommand = function(cmd) {

      private$..methodName <- "removeCommand"

      if (!c("TextCommand0") %in% class(cmd)) {
        private$..state <- paste0("Invalid text command object. Object must be ",
                                  "of the TextCmd classes.  See ?", class(self)[1],
                                  " for further assistance.")
        self$logIt("Error")
        stop()
      }

      name <- cmd$getName()
      private$..jobQueue[[name]] <- NULL

      private$..state <- paste0("Removed ", cmd$getName(), " from ", private$..x$getName(),
                                " job queue." )
      self$logIt()

      invisible(self)

    },

    execute = function() {

      private$..methodName <- "execute"

      for (i in 1:length(private$..jobQueue)) {
        private$..x <- private$..jobQueue[[i]]$new(private$..x)$execute()
      }

      private$..state <- paste0("Processed text processing commands on ",
                                private$..x$getName(), "." )
      self$logIt()

      invisible(self)

    },

    getResult = function() {
      return(private$..x)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$TextSalon(self)
    },

    #-------------------------------------------------------------------------#
    #                            Test Methods                                 #
    #-------------------------------------------------------------------------#
    exposeObject = function() {
      TextSalon <- list(
        x = private$..x,
        jobQueue = private$..jobQueue
      )
      return(TextSalon)
    }
  )
)
