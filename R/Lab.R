#==============================================================================#
#                                 Lab                                          #
#==============================================================================#
#' Lab
#'
#' \code{Lab} Class that contains document collections and the environment in which NLP happens.
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
#'  }
#'
#' \strong{Lab Aggregate Methods:}
#'  \describe{
#'   \item{\code{getChildren()}}{Retrieves a list containing meta data for child objects of the DocumentCollection class.}
#'   \item{\code{addChild(document)}}{Adds a child document, an object of the DocumentCollection class, to the Lab object.}
#'   \item{\code{removeChild(document)}}{Removes a child document, an object of the DocumentCollection class, from the Lab object.}
#'   \item{\code{parent(value)}}{Getter/setter method for the parent field, implemented as an active binding on the private member.}
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
#' @param document An object of the DocumentCollection class to be added to the Lab object's list of document collections.
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
    ..name = character(0),
    ..desc = character(0),
    ..path = character(0),
    ..parent = character(0),
    ..collections = list(),
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
        # Log Event
        historian$addEvent(className = "Lab", objectName = private$..name,
                           method = "desc",
                           event = private$..state)
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Lab Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Instantiate variables
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "lab"), desc)
      private$..path <- file.path("./NLPStudio/labs", name)
      private$..parent <- nlpStudio$getInstance()
      private$..state <- paste("Lab", name, "instantiated at", Sys.time())
      private$..modified <- Sys.time()
      private$..created <- Sys.time()

      # Create Directories
      if (!dir.exists(private$..path)) dir.create(private$..path, recursive = TRUE)
      c <- Constants$new()
      paths <- c$getLabPaths()
      lapply(paths, function(p) {
        dir <- file.path(private$..path, p)
        if (!dir.exists(dir))  dir.create(dir, recursive = TRUE)
      })

      # Initiate Log
      private$..logs <- Logger$new(file.path(private$..path, 'logs'))
      private$..logs$entry$owner <- name
      private$..logs$entry$className <- "Lab"
      private$..logs$entry$methodName <- "initialize"
      private$..logs$entry$path <- private$..path
      private$..logs$entry$level <- "Info"
      private$..logs$entry$fieldName <- NA

      # Validate Lab
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..logs$entry$msg <- private$..state <- status[['msg']]
        private$..logs$entry$level <- "Error"
        private$..logs$entry$created <- Sys.time()
        private$..logs$writeLog()
        stop()
      }

      # Create log entry
      private$..logs$entry$msg <- private$..state <- paste0("Initialized Lab: ", name)
      private$..logs$entry$created <- Sys.time()
      private$..logs$writeLog()

      # Assign its name in the global environment
      assign(name, self, envir = .GlobalEnv)

      invisible(self)
    },

    getName = function() private$..name,

    #-------------------------------------------------------------------------#
    #                         Lab Aggregate Methods                           #
    #-------------------------------------------------------------------------#
    getChildren = function() { private$..collections },

    addChild = function(child) {

      # Validation
      v <- Validator$new()
      status <- v$addChild(self, child)
      if (status[['code']] == FALSE) {
        private$..logs$entry$msg <- private$..state <- status[['msg']]
        private$..logs$entry$created <- Sys.time()
        private$..logs$writeLog()
        stop()
      }

      # Get collection information
      kidsName <- child$getName()

      # Add collection to lab's list of collections
      private$..collections[[kidsName]] <- child

      # Set parent to document collection object
      child$parent <- self

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <- private$..logs$msg <-
        paste("Collection", kidsName, "added to Lab", private$..name, "at", Sys.time())
      private$..logs$entry$created <- Sys.time()
      private$..logs$writeLog()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)

    },

    removeChild = function(child) {

      # Validation
      v <- Validator$new()
      status <- v$removeChild
      if (status[['code']] == FALSE) {
        private$..logs$entry$msg <- private$..state <- status[['msg']]
        private$..logs$entry$created <- Sys.time()
        private$..logs$writeLog()
        stop()
      }

      # Obtain collection information
      kidsName <- child$getName()

      # Remove collection from lab and update modified time
      private$..collections[[kidsName]] <- NULL

      # Change parent of removed object to null
      child$parent <- NULL

      # Update modified tieme
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <- private$..logs$msg <-
        paste("Collection", kidsName, "removed from Lab", private$..name, "at", Sys.time())
      private$..logs$entry$created <- Sys.time()
      private$..logs$writeLog()


      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      name <- visitor$getName()
      private$..state <- private$..logs$entry$msg <-
        paste("Accepted visitor,", name, "at", Sys.time())
      private$..logs$created <- Sys.time()
      private$..logs$writeLog()
      visitor$lab(self)
    },

    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      lab = list(
        name = private$..name,
        desc = private$..desc,
        parent = private$..parent,
        collections = private$..collections,
        logs <- private$..logs,
        state = private$..state,
        modified = private$..modified,
        created = private$..created
      )

      return(lab)
    }
  )
)
