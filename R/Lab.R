#==============================================================================#
#                                 Lab                                          #
#==============================================================================#
#' Lab
#'
#' \code{Lab} Class that contains document corpora and the environment in which NLP happens.
#'
#' The environment in which NLP happens. There are two groups of methods. The
#' first group allows clients to instantiate, retrieve, print, enter, leave,
#' and archive a Lab object.  The second set of methods allow clients to retrieve
#' the contained documents, add a document, and remove a document.
#'
#' \strong{Lab Core Methods:}
#'  \describe{
#'   \item{\code{new(name, desc = NULL)}}{Creates an object of Lab Class}
#'   \item{\code{desc}}{A getter/setter method allowing clients to retrieve and set the Lab description variable.}
#'   \item{\code{getName()}}{Returns the name of the Lab object.}
#'   \item{\code{getPath()}}{Returns the path of the Lab object.}
#'   \item{\code{getLogs()}}{Returns the LogR object for the Lab object.}
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#'  }
#'
#' \strong{Lab Aggregate Methods:}
#'  \describe{
#'   \item{\code{getKorpora()}}{Retrieves the list of corpora for the lab.}
#'   \item{\code{addKorpus(document)}}{Adds a corpus to the Lab object.}
#'   \item{\code{removeKorpus(document)}}{Removes a corpus from the Lab object. The corpus is archived in the NLPStudio archives.}
#' }
#'
#'
#' \strong{Lab Visitor Methods:}
#'  \describe{
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#' }
#'
#' @param name A character string containing the name of the Lab object. This variable is used in the instantiation and remove methods.
#' @param desc A chararacter string containing the description of the Lab
#' @param korpus An object of the Korpus class
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Lab <- R6::R6Class(
  classname = "Lab",
  lock_objects = FALSE,
  lock_class = FALSE,
  private = list(
    ..className = 'Lab',
    ..methodName = character(),
    ..name = character(),
    ..desc = character(),
    ..path = character(),
    ..dirs = list(),
    ..parent = character(),
    ..korpora = list(),
    ..state = character(),
    ..logs = character(),
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
                                     "Lab description changed at",
                                     Sys.time())
        self$logIt()
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Lab Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Instantiate variables
      private$..className <- 'Lab'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "lab"), desc)
      private$..path <- file.path("./NLPStudio/labs", name)
      private$..parent <- NLPStudio$new()$getInstance()
      private$..state <- paste("Lab", name, "instantiated at", Sys.time())
      private$..modified <- Sys.time()
      private$..created <- Sys.time()


      # Validate Lab
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        private$..logs <- LogR$new(file.path('./NLPStudio/logs'))
        self$logIt(level = 'Error')
        stop()
      }

      # Create lab directories.
      constants <- Constants$new()
      private$..dirs <- constants$getLabsPaths()
      lapply(private$..dirs, function(d) {
        dir <- file.path(private$..path, d)
        if (!dir.exists(dir))  dir.create(dir, recursive = TRUE)
      })


      # Create log entry
      private$..logs <- LogR$new(file.path(private$..path, 'logs'))
      self$logIt()

      # Assign its name in the global environment
      assign(name, self, envir = .GlobalEnv)

      invisible(self)
    },

    getName = function() private$..name,
    getPath = function() private$..path,
    getLogs = function() private$..logs,
    getDirs = function() private$..dirs,

    #-------------------------------------------------------------------------#
    #                         Lab Aggregate Methods                           #
    #-------------------------------------------------------------------------#
    getKorpora = function() { private$..korpora },

    addKorpus = function(korpus) {

      # Update current method
      private$..methodName <- 'addKorpus'

      # Validation
      v <- Validator$new()
      status <- v$addChild(self, korpus)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Get collection information
      korpusName <- korpus$getName()

      # Add collection to lab's list of korpora
      private$..korpora[[korpusName]] <- korpus

      # Move files to lab
      from <- korpus$getPath()
      to <- file.path(private$..path, 'corpora', korpus$getName())
      f <- FileManager$new()
      status <- f$moveFile(from, to)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Set parent to document collection object
      korpus$lab <- self

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Corpus", korpusName, "added to Lab", private$..name, "at", Sys.time())
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)

    },

    removeKorpus = function(korpus) {

      # Update current method
      private$..methodName <- 'removeKorpus'

      # Validation
      v <- Validator$new()
      status <- v$removeChild(self, korpus)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Obtain collection information
      korpusName <- korpus$getName()

      # Remove collection from lab and update modified time
      private$..korpora[[korpusName]] <- NULL

      # Change parent of removed object to null
      korpus$lab <- NULL

      # Move files back to korpus directory
      from <- file.path(private$..dirs$korpora, korpusName)
      to <- NLPStudio$new()$getInstance()$getDirs()$korpora
      f <- FileManager$new()
      status <- f$moveFile(from, to)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Korpus", korpusName, "removed from Lab", private$..name, "at", Sys.time())
      self$logIt()

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      name <- visitor$getName()
      visitor$lab(self)
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

      lab = list(
        name = private$..name,
        desc = private$..desc,
        path = private$..path,
        dirs = private$..dirs,
        parent = private$..parent,
        korpora = private$..korpora,
        logs <- private$..logs,
        state = private$..state,
        modified = private$..modified,
        created = private$..created
      )

      return(lab)
    }
  )
)
