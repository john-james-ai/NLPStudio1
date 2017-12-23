#==============================================================================#
#                                 Meta                                         #
#==============================================================================#
#' Meta
#'
#' \code{Meta} Abstract class for the Metadata classes.
#'
#' Abstract class for the Metadata classes. Defines methods for adding, and
#' manipulating meta data for entities within the package.
#'
#' @section Meta core methods:
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a document. Not implemented for the abstract class.}
#'   \item{\code{getName()}}{Method that returns the name of the Meta object. }
#'   \item{\code{getFileName()}}{Method for obtaining the document file name. .}
#'   \item{\code{getPath()}}{Method for obtaining the document path. }
#'  }
#'
#'
#' @param name Character string indicating the file path for a document
#' @param path Character string indicating the path to the docment file.
#' @param parent Corpus object to which the document is composed.
#' @param content List containing character vectors of text.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Meta classes
#' @export
Meta <- R6::R6Class(
  classname = "Meta",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..dublincore = list(
      contributor = character(),
      coverage = character(),
      creator = character(),
      date = character(),
      description = character(),
      format = character(),
      identifier = character(),
      language = character(),
      publisher = character(),
      relation = character(),
      rights = character(),
      source = character(),
      subject = character(),
      title = character(),
      type = character()
    ),
    ..content = list(),
    ..file = character()
  ),

  active = list(
    content = function(value = NULL) {

      if (missing(value)) {
        private$..admin$accessed <- Sys.time()
        return(private$..content)
      } else {
        if (is.null(private$..content)) private$..admin$created <- Sys.time()
        private$..content <- value
        private$..admin$modified <- Sys.time()
        private$..admin$accessed <- Sys.time()
      }
    },
    contributor = function(value = NULL) return(private$..dublincore$contributor   <- ifelse(is.null(value),  private$..dublincore$contributor,   value)),
    coverage = function(value = NULL) return(private$..dublincore$coverage   <- ifelse(is.null(value),  private$..dublincore$coverage,   value)),
    creator = function(value = NULL) return(private$..dublincore$creator   <- ifelse(is.null(value),  private$..dublincore$creator,   value)),
    date = function(value = NULL) return(private$..dublincore$date   <- ifelse(is.null(value),  private$..dublincore$date,   value)),
    description = function(value = NULL) return(private$..dublincore$description   <- ifelse(is.null(value),  private$..dublincore$description,   value)),
    format = function(value = NULL) return(private$..dublincore$format   <- ifelse(is.null(value),  private$..dublincore$format,   value)),
    identifier = function(value = NULL) return(private$..dublincore$identifier   <- ifelse(is.null(value),  private$..dublincore$identifier,   value)),
    language = function(value = NULL) return(private$..dublincore$language   <- ifelse(is.null(value),  private$..dublincore$language,   value)),
    publisher = function(value = NULL) return(private$..dublincore$publisher   <- ifelse(is.null(value),  private$..dublincore$publisher,   value)),
    relation = function(value = NULL) return(private$..dublincore$relation   <- ifelse(is.null(value),  private$..dublincore$relation,   value)),
    rights = function(value = NULL) return(private$..dublincore$rights   <- ifelse(is.null(value),  private$..dublincore$rights,   value)),
    source = function(value = NULL) return(private$..dublincore$source   <- ifelse(is.null(value),  private$..dublincore$source,   value)),
    subject = function(value = NULL) return(private$..dublincore$subject   <- ifelse(is.null(value),  private$..dublincore$subject,   value)),
    title = function(value = NULL) return(private$..dublincore$title   <- ifelse(is.null(value),  private$..dublincore$title,   value)),
    type = function(value = NULL) return(private$..dublincore$type   <- ifelse(is.null(value),  private$..dublincore$type,   value))
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name, file = NULL) {

      # Instantiate variables
      private$..admin$className <- 'Meta'
      private$..admin$methodName <- 'initialize'
      private$..admin$name <- name
      private$..admin$state <- paste0("Meta, ", private$..admin$name, ", instantiated.")
      private$..admin$logs <- LogR$new()
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()

      private$..file <- file
      if (!is.null(file)) {
        private$..content <- file$read()
        file$flush()
      }

      # Validate Meta
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..admin$state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Create log entry
      self$logIt()

      invisible(self)
    },

    getName = function() private$..admin$name,
    getFile = function() private$..file,

    #-------------------------------------------------------------------------#
    #                         Content Methods                                 #
    #-------------------------------------------------------------------------#
    read = function() {

      private$..admin$methodName <- 'read'

      if (private$..file$fileInfo()$mtime < private$..admin$modified &
          !is.null(private$..content)) {
        content <- private$..content
      } else {
        content <- private$..file$read()
      }

      # LogIt
      private$..admin$state <- paste0("Read ", private$..admin$name, ". ")
      private$..admin$accessed <- Sys.time()
      self$logIt()

      return(content)
    },

    write = function() {

      private$..admin$methodName <- 'write'

      private$..file$content <- private$..content
      private$..file$write()

      # LogIt
      private$..admin$state <- paste0("Wrote ", private$..admin$name, ". ")
      private$..admin$accessed <- Sys.time()
      self$logIt()

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$document(self)
    },

    #-------------------------------------------------------------------------#
    #                           Expose Object                                 #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        admin = list(
          name = private$..admin$name,
          path = private$..admin$path,
          className = private$..admin$className,
          methodName = private$..admin$methodName,
          locked = private$..admin$locked,
          state = private$..admin$state,
          logs = private$..admin$logs,
          created = private$..admin$created,
          modified = private$..admin$modified,
          accessed = private$..admin$accessed
          ),
        dublincore = list(
          contributor = private$..meta$contributor,
          coverage = private$..meta$coverage,
          creator = private$..meta$creator,
          date = private$..meta$date,
          description = private$..meta$description,
          format = private$..meta$format,
          identifier = private$..meta$identifier,
          language = private$..meta$language,
          publisher = private$..meta$publisher,
          relation = private$..meta$relation,
          rights = private$..meta$rights,
          source = private$..meta$source,
          subject = private$..meta$subject,
          title = private$..meta$title,
          type = private$..meta$type
          ),
        content = private$..content,
        file = private$..file
        )
      return(o)
    }

  )
)
