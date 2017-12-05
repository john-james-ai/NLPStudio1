#==============================================================================#
#                                 Lab                                          #
#==============================================================================#
#' Lab
#'
<<<<<<< HEAD
#' \code{Lab} Class that contains document corpora and the environment in which NLP happens.
=======
#' \code{Lab} Class that contains document collections and the environment in which NLP happens.
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
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
<<<<<<< HEAD
#'   \item{\code{getPath()}}{Returns the path of the Lab object.}
#'   \item{\code{getLogs()}}{Returns the LogR object for the Lab object.}
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
=======
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
#'  }
#'
#' \strong{Lab Aggregate Methods:}
#'  \describe{
<<<<<<< HEAD
#'   \item{\code{getKorpora()}}{Retrieves the list of corpora for the lab.}
#'   \item{\code{addKorpus(document)}}{Adds a corpus to the Lab object.}
#'   \item{\code{removeKorpus(document)}}{Removes a corpus from the Lab object. The corpus is archived in the NLPStudio archives.}
=======
#'   \item{\code{getChildren()}}{Retrieves a list containing meta data for child objects of the DocumentCollection class.}
#'   \item{\code{addChild(document)}}{Adds a child document, an object of the DocumentCollection class, to the Lab object.}
#'   \item{\code{removeChild(document)}}{Removes a child document, an object of the DocumentCollection class, from the Lab object.}
#'   \item{\code{parent(value)}}{Getter/setter method for the parent field, implemented as an active binding on the private member.}
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
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
<<<<<<< HEAD
#' @param korpus An object of the Korpus class
=======
#' @param document An object of the DocumentCollection class to be added to the Lab object's list of document collections.
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
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
<<<<<<< HEAD
    ..className = 'Lab',
    ..methodName = character(),
    ..name = character(),
    ..desc = character(),
    ..path = character(),
    ..dirs = list(),
    ..parent = character(),
    ..korpora = list(),
=======
    ..name = character(0),
    ..desc = character(0),
    ..path = character(0),
    ..parent = character(0),
    ..collections = list(),
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
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
<<<<<<< HEAD
        self$logIt()
=======
        # Log Event
        historian$addEvent(className = "Lab", objectName = private$..name,
                           method = "desc",
                           event = private$..state)
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Lab Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Instantiate variables
<<<<<<< HEAD
      private$..className <- 'Lab'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "lab"), desc)
      private$..path <- file.path("./NLPStudio/labs", name)
      private$..parent <- NLPStudio$new()$getInstance()
=======
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "lab"), desc)
      private$..path <- file.path("./NLPStudio/labs", name)
      private$..parent <- nlpStudio$getInstance()
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
      private$..state <- paste("Lab", name, "instantiated at", Sys.time())
      private$..modified <- Sys.time()
      private$..created <- Sys.time()

<<<<<<< HEAD
=======
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
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900

      # Validate Lab
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
<<<<<<< HEAD
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
=======
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
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900

      # Assign its name in the global environment
      assign(name, self, envir = .GlobalEnv)

      invisible(self)
    },

    getName = function() private$..name,
<<<<<<< HEAD
    getPath = function() private$..path,
    getLogs = function() private$..logs,
    getDirs = function() private$..dirs,
=======
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900

    #-------------------------------------------------------------------------#
    #                         Lab Aggregate Methods                           #
    #-------------------------------------------------------------------------#
<<<<<<< HEAD
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
=======
    getChildren = function() { private$..collections },

    addChild = function(child) {

      # Validation
      v <- Validator$new()
      status <- v$addChild(self, child)
      if (status[['code']] == FALSE) {
        private$..logs$entry$msg <- private$..state <- status[['msg']]
        private$..logs$entry$created <- Sys.time()
        private$..logs$writeLog()
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
        stop()
      }

      # Get collection information
<<<<<<< HEAD
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
=======
      kidsName <- child$getName()

      # Add collection to lab's list of collections
      private$..collections[[kidsName]] <- child

      # Set parent to document collection object
      child$parent <- self
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
<<<<<<< HEAD
      private$..state <-
        paste("Corpus", korpusName, "added to Lab", private$..name, "at", Sys.time())
      self$logIt()
=======
      private$..state <- private$..logs$msg <-
        paste("Collection", kidsName, "added to Lab", private$..name, "at", Sys.time())
      private$..logs$entry$created <- Sys.time()
      private$..logs$writeLog()
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)

    },

<<<<<<< HEAD
    removeKorpus = function(korpus) {

      # Update current method
      private$..methodName <- 'removeKorpus'

      # Validation
      v <- Validator$new()
      status <- v$removeChild(self, korpus)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
=======
    removeChild = function(child) {

      # Validation
      v <- Validator$new()
      status <- v$removeChild
      if (status[['code']] == FALSE) {
        private$..logs$entry$msg <- private$..state <- status[['msg']]
        private$..logs$entry$created <- Sys.time()
        private$..logs$writeLog()
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
        stop()
      }

      # Obtain collection information
<<<<<<< HEAD
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
=======
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

>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      name <- visitor$getName()
<<<<<<< HEAD
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
=======
      private$..state <- private$..logs$entry$msg <-
        paste("Accepted visitor,", name, "at", Sys.time())
      private$..logs$created <- Sys.time()
      private$..logs$writeLog()
      visitor$lab(self)
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
    },

    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      lab = list(
        name = private$..name,
        desc = private$..desc,
<<<<<<< HEAD
        path = private$..path,
        dirs = private$..dirs,
        parent = private$..parent,
        korpora = private$..korpora,
=======
        parent = private$..parent,
        collections = private$..collections,
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
        logs <- private$..logs,
        state = private$..state,
        modified = private$..modified,
        created = private$..created
      )

      return(lab)
    }
  )
)
