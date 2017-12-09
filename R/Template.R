#==============================================================================#
#                                 Template                                     #
#==============================================================================#
#' Template
#'
#' \code{Template} Template for class definitions
#'
#' More descriptive text
#'
#' @section Class participants and collaborators:
#'
#' \strong{Family of Classes Participants:}
#' The participants of the ...  class are:
#' \itemize{
#'  \item Template: This component class specifies an abstract interface
#'  for all leaf and composite document classes.
#'  }
#'
#' \strong{Family of Classes Collaborators:}
#' The collaborators of the .... family  are:
#'  \itemize{
#'   \item Lab: Class responsible for document collections.
#'  }
#'
#' @section Class methods:
#'
#' \strong{Template Methods:}
#' There are six types of methods within the Template class and they are:
#' \itemize{
#'  \item{Core Methods: Core methods shared by both Document and
#'  DocumentCollection objects.}
#'  \item{Getter/Setter Methods: Active binding methods for getting and setting
#'  selected private members.}
#'  \item{Composite Methods: Methods implemented by the DocumentCollection
#'  class to maintain the document heirarchy.}
#'  \item{State Methods: Methods for saving current and restoring prior states of objects .}
#'  \item{Visitor Methods: Methods for implementation of and messaging
#'  with objects of the visitor classes.}
#' }
#'
#' \strong{Template Core Methods:}
#'  \itemize{
#'   \item{\code{new(name, desc)}}{Base method for instantiating
#'   an object of the Document or DocumentCollection classes.
#'   Specific behaviors implemented in the subclasses. }
#'   \item{\code{getName()}}{Returns the name of the current object.}
#'   \item{\code{exposeObject(requester)}}{Base method for returning the elements of the current object if invoked by an authorized method.}
#'   \item{\code{restore(requester, prior)}}{Base method for restoring an object
#'   to a prior state, as per the object parameter.}
#' }
#'
#' \strong{Template Field Getter/Setter Active Binding Methods:}
#'  \itemize{
#'   \item{\code{desc()}}{Method used to get / set the description variable.
#'   Implemented as an active binding and so the field may be updated
#'   by assignment. This method is concrete and inherited by sub-classes.}
#' }
#'
#' \strong{Template Composite Methods:}
#'  \itemize{
#'   \item{\code{addChild(document)}}{Base method for adding documents to a
#'   collection. Specific behaviors implemented in the DocumentCollection composite
#'   sub-class}
#'   \item{\code{getChildren()}}{Base method for retrieving child objects. Specific behaviors
#'   implemented in the DocumentCollection subclass }
#'   \item{\code{removeChild(document)}}{Base method for removing documents from
#'   a collection. Specific behaviors implemented in the DocumentCollection composite
#'   sub-class}
#'   \item{\code{parent(value)}}{Getter/setter method for the parent field, implemented as an active binding on the private member.}
#' }
#'
#' \strong{Template State Methods:}
#'  \itemize{
#'   \item{\code{saveState()}}{Method for saving the current state of an object to file.}
#'   \item{\code{restoreState(prior)}}{Method for restoring an object to a prior state.}
#'  }
#'
#' \strong{Template Visitor Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Method for accepting the visitor objects. Subclasses override these methods.}
#' }
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Template <- R6::R6Class(
  classname = "Template",
  lock_objects = TRUE,
  lock_class = TRUE,
  private = list(
    ..name = character(),
    ..desc = character(),
    ..path = character(),
    ..log = character(),
    ..state = character(),
    ..created = "None",
    ..modified = "None"
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function() {

      # Create logger and initialization log entry
      private$..log <- LogR$new()
      private$..log$entry$owner <- private$..name
      private$..log$entry$className <- "Lab"
      private$..log$entry$methodName <- "initialize"
      private$..log$entry$path <- private$..path
      private$..log$entry$level <- "Info"
      private$..log$entry$msg <- paste("Initialized", private$..name, "lab.")
      private$..log$entry$fieldName <- private$..name
      private$..log$entry$created <- Sys.time()
      private$..log$writeLog()

      # Assign its name in the global environment
      assign(name, self, envir = .GlobalEnv)

      invisible(self)
    },

    getName = function() {
      return(private$..name)
    },


    #-------------------------------------------------------------------------#
    #                            Log Method                                   #
    #-------------------------------------------------------------------------#
    logIt = function(level = 'Info', fieldName = NA) {

      private$..logs$entry$owner <- private$..name
      private$..logs$entry$className <- "Lab"
      private$..logs$entry$methodName <- match.call()[[1]]
      private$..logs$entry$level <- level
      private$..logs$entry$msg <- private$..state
      private$..logs$entry$fieldName <- fieldName
      private$..logs$created <- Sys.time()
      private$..logs$writeLog()
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      name <- visitor$getName()
      private$..state <- paste("Accepted visitor,", name, "at", Sys.time())
      self$logIt()
      visitor$template(self)
    },

    #-------------------------------------------------------------------------#
    #                             Test Methods                                #
    #-------------------------------------------------------------------------#
    exposeObject = function() {
      o <- list(
        name <- private$..name,
        desc <- private$..desc,
        path <- private$..path,
        log <- private$..log,
        created <- private$..created,
        modified <- private$..modified
      )
      return(o)
    }

  )
)
