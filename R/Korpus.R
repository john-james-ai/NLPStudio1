#==============================================================================#
#                                 Korpus                                       #
#==============================================================================#
#' Korpus
#'
#' \code{Korpus} Class that defines a corpus or collection of documents
#'
#' The class one or several documents in text form, as well as various
#' transformations such as nGrams and POS tags
#'
#' @section Korpus Core Methods:
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
#' @section Korpus Processing Methods:
#'  \describe{
#'   \item{\code{obtainKorpus()}}{Method for initiating the repair operation on a document.}
#'   \item{\code{repairKorpus()}}{Method for initiating the repair operation on a document.}
#'   \item{\code{splitCorpus()}}{Method for initiating the repair operation on a document.}
#' }
#'
#' @param name A character string containing the name of the Korpus object. This variable is used in the instantiation and remove methods.
#' @param desc A chararacter string containing the description of the Korpus.
#' @param document An object of one of the Document sub-classes.
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Korpus <- R6::R6Class(
  classname = "Korpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  private = list(
    ..className = 'Korpus',
    ..methodName = character(),
    ..name = character(),
    ..desc = character(),
    ..parent = character(),
    ..path = character(),
    ..dirs = list(),
    ..external = list(),
    ..raw = list(),
    ..sets = list(),
    ..logs = character(),
    ..reports = list(),
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

    lab = function(value) {
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
                                      value$getDirs()$korpora, private$..name)
          private$..logs <- LogR$new(file.path(value$getPath(),
                                               value$getDirs()$korpora,
                                               private$..name, 'logs'))
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
    #                         Korpus Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Instantiate variables
      private$..className <- 'Korpus'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "corpus"), desc)
      private$..path <- file.path(NLPStudio$new()$getInstance()$getDirs()$korpora, name)
      private$..state <- paste("Corpus", name, "instantiated.")
      private$..modified <- Sys.time()
      private$..created <- Sys.time()

      # Validate Korpus
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        private$..logs <- LogR$new(file.path(NLPStudio$new()$getInstance()$getDirs()$logs))
        self$logIt(level = 'Error')
        stop()
      }

      # Create directories
      constants <- Constants$new()
      private$..dirs <- constants$getKorpusPaths()
      lapply(private$..dirs, function(d) {
        dir <- file.path(private$..path, d)
        if (!dir.exists(dir))  dir.create(dir, recursive = TRUE)
      })

      # Log it
      private$..logs <- LogR$new(file.path(private$..path, 'logs'))
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    getName = function() private$..name,
    getPath = function() private$..path,

    #-------------------------------------------------------------------------#
    #                         Korpus Initialization                           #
    #-------------------------------------------------------------------------#
    downloadKorpus = function(url) {
      downloadPath <- file.path(private$..path, private$..dirs$external)
      f <- FileManager$new()
      status <- f$download(url, downloadPath)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }
    },

    addDocument = function(filePath) {

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

      Korpus = list(
        className = private$..className,
        methodName = private$..methodName,
        name = private$..name,
        desc = private$..desc,
        path = private$..path,
        parent = private$..parent,
        external = private$..external,
        raw = private$..raw,
        preprocessed = private$..preprocessed,
        sets = private$..sets,
        logs = private$..logs,
        reports = private$..reports,
        state = private$..state,
        modified = private$..modified,
        created = private$..created
      )

      return(Korpus)
    }
  )
)
