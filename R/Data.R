#==============================================================================#
#                                 Data                                         #
#==============================================================================#
#' Data
#'
#' \code{Data} Class containing the data for a pipeline.
#'
#' Class contains the external, raw, refined, and processed cross validation
#' sets.
#'
#' @section Data Methods:
#'  \describe{
#'   \item{\code{new(name, path)}}{Instantiates a Data object and initiatesa Data object. }
#'   \item{\code{getCorpora()}}{Retrieves the Data object's list of Corpus objects .}
#'   \item{\code{addCorpus(corpus)}}{Adds a corpus to the Data object .}
#'   \item{\code{removeCorpus(corpus)}}{Removes a Corpus object from the Data object.}
#'   \item{\code{logIt(level = "Info")}}{Logs an event for the Data object.}
#'  }
#'
#' @section Parameters:
#' @param corpus Corpus object
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Data <- R6::R6Class(
  classname = "Data",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..collections = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Data Core Methods                               #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) {

      private$..className <- 'Data'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..path <- path
      private$..state <- paste("Data object", private$..name, "instantiated.")
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()

      dir.create(private$..path, showWarnings = FALSE, recursive = TRUE)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                          Aggegate Methods                               #
    #-------------------------------------------------------------------------#
    getCollections = function() private$..collections,

    addCollection = function(collection) {
      name <- collection$getName()
      private$..collections[[name]] <- collection
      private$..state <- paste0("Added ", class(collection)[1], " object, ",
                                name, ", to the data set.")
      self$logIt()

      invisible(self)
    },

    removeCollection = function(collection) {
      name <- collection$getName()
      private$..collections[[name]] <- NULL
      private$..state <- paste0("Removed ", class(collection)[1], " object, ",
                                name, ", from the data set.")
      self$logIt()

      invisible(self)
    }
  )
)
