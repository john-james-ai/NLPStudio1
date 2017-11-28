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
#'   \item{\code{exposeObject(requester)}}{Returns object elements in a list format to authorized requester.}
#'   \item{\code{restore(requester, prior)}}{Restores an object to a prior state, if invoked by authorized requester.}
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
#' \strong{State Methods:}
#'  \describe{
#'   \item{\code{saveState()}}{Retrieves a list containing meta data for child objects of the DocumentCollection class.}
#'   \item{\code{restoreState()}}{Removes a child document, an object of the DocumentCollection class, from the Lab object.}
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
#' @param stateId Character string identifying a prior stateDesc for a Lab object.
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
    ..parent = character(0),
    ..collections = list(),
    ..stateId = character(0),
    ..stateDesc = character(0),
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
        private$..stateDesc <- paste(private$..name,
                                     "Lab description changed at",
                                     Sys.time())
        # self$saveState()

        # Log Event
        historian$addEvent(className = "Lab", objectName = private$..name,
                           method = "desc",
                           event = private$..stateDesc)
      }

    },

    parent = function(value) {
      if (missing(value)) {
        private$..parent
      } else {
        v <- Validator$new()
        if (v$setParent(self, value) == FALSE) {
          stop()
        }
        # Before
        private$..stateDesc <- paste("Memento of ",private$..name, "prior to",
                                     "changing its parent at", Sys.time())
        # self$saveState()

        private$..parent <- value
        private$..modified <- Sys.time()

        # After
        name <- ifelse(is.null(value), "NULL", value$getName())
        private$..stateDesc <- paste(private$..name, "parent changed to",
                                     name, "at", Sys.time())
        # self$saveState()

        # Log Event
        historian$addEvent(className = "Lab", objectName = private$..name,
                           method = "parent",
                           event = private$..stateDesc)
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
      private$..desc <- ifelse(is.null(desc), paste(name, "Lab"), desc)
      private$..parent <- nlpStudio$getInstance()
      private$..stateDesc <- paste("Lab", name, "instantiated at", Sys.time())
      private$..modified <- Sys.time()
      private$..created <- Sys.time()

      # Validate Lab
      v <- Validator$new()
      if (v$init(self) == FALSE) stop()

      # Assign its name in the global environment
      assign(name, self, envir = .GlobalEnv)

      # Log Event
      historian$addEvent(className = "Lab", objectName = name,
                         method = "initialize",
                         event = private$..stateDesc)

      invisible(self)
    },

    getName = function() {
      return(private$..name)
    },

    exposeObject = function(requester) {

      # TODO; Uncomment after testing
      # v <- Validator()
      # if (v$exposeObject(object = self,
      #                 requester = requester) == FALSE) stop()

      lab = list(
        name = private$..name,
        desc = private$..desc,
        parent = private$..parent,
        collections = private$..collections,
        stateDesc = private$..stateDesc,
        stateId = private$..stateId,
        modified = private$..modified,
        created = private$..created
      )

      return(lab)
    },

    restore = function(requester, prior) {

      v <- Validator$new()
      if (v$restore(object = self,
                    requester = requester, prior = prior) == FALSE) stop()

      r <- restored$exposeObject()
      private$..desc <- r$desc
      private$..parent <- r$parent
      private$..collections <- r$collections
      private$..stateDesc <- paste("Lab object", private$..name,
                                   "restored to prior state designated",
                                   "by state id:", r$stateId, "at",
                                   system.time())
      private$..stateId <- r$stateId
      private$..created <- r$created
      private$..modified <- Sys.time()

      # Log event
      # historian$addEvent(className = class(self)[1], objectName = name,
      #                    method = "restore",
      #                    event = private$..stateDesc)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                         Lab Aggregate Methods                           #
    #-------------------------------------------------------------------------#
    getChildren = function() { private$..collections },

    addChild = function(child) {

      # Validation
      v <- Validator$new()
      if (v$addChild(object = self, child = child) == FALSE) stop()

      # Get collection information
      kidsName <- child$getName()

      # Save state as memento
      private$..stateDesc <- paste("Memento of", private$..name,
                                   "before adding ", kidsName, "at", Sys.time())
      # private$saveState(self)

      # Add collection to lab's list of collections
      private$..collections[[kidsName]] <- child

      # Set parent to document collection object
      child$parent <- self

      # Update modified time
      private$..modified <- Sys.time()

      # Save State
      private$..stateDesc <- paste("Collection", kidsName, "added to Lab", private$..name, "at", Sys.time())
      # private$saveState(self)

      # Log Event
      historian$addEvent(className = "Lab", objectName = private$..name,
                         method = "addChild",
                         event = private$..stateDesc)

      invisible(self)

    },

    removeChild = function(child) {

      # Validation
      v <- Validator$new()
      if (v$removeChild(object = self, child = child) == FALSE) stop()

      # Obtain collection information
      kidsName <- child$getName()

      # Save state as memento
      private$..stateDesc <- paste("Memento of", private$..name, "before removing ", kidsName, "at", Sys.time())
      # private$saveState(self)


      # Remove collection from lab and update modified time
      private$..collections[[kidsName]] <- NULL

      # Change parent of removed object to null
      child$parent <- NULL

      # Update modified tieme
      private$..modified <- Sys.time()

      # Update State
      private$..stateDesc <- paste("Collection", kidsName, "removed from Lab", private$..name, "at", Sys.time())
      # private$saveState(self)

      # Log Event
      historian$addEvent(className = "Lab", objectName = private$..name,
                         method = "removeChild",
                         event = private$..stateDesc)

      invisible(self)

    },

    #-------------------------------------------------------------------#
    #                           State Method                            #
    #-------------------------------------------------------------------#
    saveState = function() {
      state <- State$new()
      private$..stateId <- state$save(self)
    },

    restoreState = function(stateId) {
      private$..stateId <- stateId
      state <- State$new()
      state$restore(self)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$lab(self)
    }
  )
)
