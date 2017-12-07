#==============================================================================#
#                                 Corpus                                       #
#==============================================================================#
#' Corpus
#'
#' \code{Corpus} Class that defines a corpus or collection of documents
#'
#' The class one or several documents in text form, as well as various
#' transformations such as nGrams and POS tags
#'
#' @section Corpus Core Methods:
#'  \describe{
#'   \item{\code{new(name, desc = NULL, lab = NULL)}}{Creates an object of Corpus Class}
#'   \item{\code{desc}}{A getter/setter method allowing clients to retrieve and set the Corpus description variable.}
#'   \item{\code{lab}}{A getter/setter method allowing clients to retrieve and set the Lab object to which the Corpus object belongs.}
#'   \item{\code{getName()}}{Returns the name of the Corpus object.}
#'   \item{\code{getPath()}}{Returns the path of the Corpus object.}
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#'  }
#'
#' @section Corpus Processing Methods:
#'  \describe{
#'   \item{\code{obtainCorpus()}}{Method for initiating the repair operation on a document.}
#'   \item{\code{repairCorpus()}}{Method for initiating the repair operation on a document.}
#'   \item{\code{splitCorpus()}}{Method for initiating the repair operation on a document.}
#' }
#'
#' @param name A character string containing the name of the Corpus object. This variable is used in the instantiation and remove methods.
#' @param desc A chararacter string containing the description of the Corpus.
#' @param document An object of one of the Document sub-classes.
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Corpus <- R6::R6Class(
  classname = "Corpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  private = list(
    ..className = 'Corpus',
    ..methodName = character(),
    ..name = character(),
    ..desc = character(),
    ..parent = character(),
    ..path = character(),
    ..extDocs = list(),
    ..rawDocs = list(),
    ..state = character(),
    ..modified = "None",
    ..created = "None"
  ),

  active = list(

    desc = function(value) {
      if (missing(value)) {
        private$..desc
      } else {
        private$..desc <- value
        private$..modified <- Sys.time()
        private$..state <- paste(private$..name,
                                     "Corpus description changed at",
                                     Sys.time())
        self$logIt()
      }
    },

    parent = function(value) {
      if (missing(value)) {
        private$..parent
      } else {
        v <- Validator$new()
        status <- v$setParent(self, value)
        if (status[['code']] == FALSE) {
          private$..state <- status[['msg']]
          self$logIt(level = 'Error')
          stop()
        } else {
          private$..parent <- value
          private$..path <- file.path(value$getPath(),
                                      private$..name)
          private$..modified <- Sys.time()
          private$..state <- paste0('Corpus ', private$..name, 'added to ',
                                    value$getName(), ' lab. ')
          self$logIt()

          # Assign its name in the global environment
          assign(private$..name, self, envir = .GlobalEnv)

          invisible(self)

        }
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Corpus Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL, extDocs = NULL, rawDocs = NULL,
                          preDocs = NULL) {

      # Instantiate variables
      private$..className <- 'Corpus'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "corpus"), desc)
      private$..parent <- NLPStudio$new()$getInstance()
      private$..path <- file.path(NLPStudio$new()$getInstance()$getDirs()$korpora, private$..name)
      private$..state <- "Corpus instantiated."
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..logs <- LogR$new(NLPStudio$new()$getInstance()$getDirs()$logs)
      private$..extDocs <- extDocs
      private$..rawDocs <- rawDocs
      private$..preDocs <- preDocs

    },

    #-------------------------------------------------------------------------#
    #                         Corpus Initialization                           #
    #-------------------------------------------------------------------------#


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

      Corpus = list(
        className = private$..className,
        methodName = private$..methodName,
        name = private$..name,
        desc = private$..desc,
        path = private$..path,
        parent = private$..parent,
        external = private$..external,
        raw = private$..raw,
        sets = private$..sets,
        logs = private$..logs,
        reports = private$..reports,
        state = private$..state,
        modified = private$..modified,
        created = private$..created
      )

      return(Corpus)
    }
  )
)
