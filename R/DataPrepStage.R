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
    ..data = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Data Core Methods                               #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) {

      private$..admin$className <- 'Data'
      private$..admin$methodName <- 'initialize'
      private$..name <- name
      private$..admin$path <- path
      private$..admin$state <- paste("Data object", private$..name, "instantiated.")
      private$..admin$logs <- LogR$new()
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()

      dir.create(private$..admin$path, showWarnings = FALSE, recursive = TRUE)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                          Aggegate Methods                               #
    #-------------------------------------------------------------------------#
    getData = function() private$..data,

    addData = function(data) {
      name <- data$getName()
      private$..data[[name]] <- data
      private$..admin$state <- paste0("Added ", class(data)[1], " object, ",
                                name, ", to the data set.")
      self$logIt()

      invisible(self)
    },

    removeData = function(data) {
      name <- data$getName()
      private$..data[[name]] <- NULL
      private$..admin$state <- paste0("Removed ", class(data)[1], " object, ",
                                name, ", from the data set.")
      self$logIt()

      invisible(self)
    }
  )
)
