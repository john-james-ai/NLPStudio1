#==============================================================================#
#                               DocumentXlsx                                   #
#==============================================================================#
#' DocumentXlsx
#'
#' \code{DocumentXlsx} Class for instantiating, reading, and writing objects
#' of the DocumentXlsx class..
#'
#' \strong{Document Family of Classes Overview:}
#'
#' The Document family of classes is an implementation of the composite
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows composite
#' and individual objects to be treated uniformly.
#'
#' The following sections include:
#' \itemize{
#'  \item Class Participants: Classes the comprise this composite pattern.
#'  \item Class Collaborators: Classes which interact with the Document0 class.
#'  \item Class Methods: Methods included in the interface.
#'  }
#'
#' \strong{Document Family of Classes Participants:}
#' The participants of the Document0 class are:
#' \itemize{
#'  \item Document0: This component class specifies an abstract interface
#'  for all leaf and composite document classes.
#'  \item DocumentCollection: The composite class that maintains the
#'  hierarchical structure of document collections (composites) and individual
#'  documents (leafs).
#'  \item Document: This "abstract leaf" class specifies the concrete class for
#'  individual documents.
#'  \item DocumentText: This "concrete leaf" class for text documents.
#'  \item DocumentCsv: This "concrete leaf" class for csv documents.
#'  \item DocumentRdata: This "concrete leaf" class for RData documents.
#'  \item DocumentXlsx: This "concrete leaf" class for excel documents.
#'  }
#'
#' \strong{DocumentXlsx Class Collaborators:}
#' The collaborators of the Document family  are:
#'  \itemize{
#'   \item State: Class responsible for saving current and restoring prior states of objects.
#'   \item Historian: Class responsible for maintaining the history of events on objects.
#'   \item Reader: Class responsible for initiating the document read operation.
#'   \item Writer: Class responsible for initiating the document write operation.
#'
#'  }
#'
#' \strong{DocumentXlsx Methods:}
#' There are six types of methods within the Document class and they are:
#' \itemize{
#'  \item{Core Methods: Core methods shared by both Document and
#'  DocumentCollection objects.}
#'  \item{Getter/Setter Methods: Active binding methods for getting and setting
#'  selected private members.}
#'  \item{Composite Methods: Methods implemented by the DocumentCollection
#'  class to maintain the document heirarchy.}
#'  \item{State Methods: Methods for saving current and restoring to prior object states.}
#'  \item{Visitor Methods: Methods for implementation of and messaging
#'  with objects of the visitor classes.}
#' }
#'
#' \strong{DocumentXlsx Core Methods:}
#'  \itemize{
#'   \item{\code{new(name, desc)}}{Method for instantiating a document}
#'   \item{\code{getName()}}{Method for returning the name of the current document.}
#'   \item{\code{restore(curator, prior)}}{Method for restoring an object to a prior state, as per the object parameter.}
#'   \item{\code{addContent(content)}}{Method for adding content to the document object. This method is invoked by the read visitor.}
#'  }
#'
#' \strong{DocumentXlsx Field Getter/Setter Active Binding Methods:}
#'  \itemize{
#'   \item{\code{desc()}}{Method used to get / set the description variable.
#'   Implemented as an active binding and so the field may be updated
#'   by assignment. This method is inherited from the Document0 class.}#'
#' }
#'
#' \strong{DocumentXlsx Composite Methods:}
#'  \itemize{
#'   \item{\code{addChild(document)}}{Not implemented for this class.}
#'   \item{\code{getChildren()}}{Returns NULL.}
#'   \item{\code{removeChild(document)}}{Not implemented for this class.}
#'   \item{\code{parent(value)}}{Getter/setter method for the parent field, implemented as an active binding on the private member.}
#' }
#'
#'
#' \strong{DocumentXlsx State Methods:}
#'  \itemize{
#'   \item{\code{saveState()}}{Method that initiates the process of saving the current state of the object. This method is inherited from the Document class.}
#'   \item{\code{restoreState()}}{Method that initiates the process of restoring an object to a prior state. This method is inherited from the Document class.}
#'  }
#'
#' \strong{DocumentXlsx Visitor Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Method for accepting the visitor objects.}
#'  }
#'
#' @param name Character string indicating the name of the document or file. Required for all objects.
#' @param desc Character string containing the description of the document.
#' @param content Nested list of content to be written to files.
#' @param path Character string containing the path to the document.
#' @param fileName Character string indicating File object's file name.
#' @param parent An object of the Lab or DocumentCollection class that represents
#' the parent object.
#' @param visitor An object of one of the visitor classes.
#' @param stateId Character string that uniquely identifies an object and its
#' state at a specific point in time.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document classes
#' @export
DocumentXlsx <- R6::R6Class(
  classname = "DocumentXlsx",
  inherit = Document,
  lock_objects = FALSE,
  lock_class = FALSE,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name, fileName, path, desc = NULL) {

      # Instantiate variables
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "Excel Document"), desc)
      private$..path <- path
      private$..fileName <- fileName
      private$..stateDesc <- paste("DocumentXlsx object,", name, "instantiated at", Sys.time())
      private$..created <- Sys.time()
      private$..modified <- Sys.time()

      # Validate Document
      v <- Validator$new()
      if (v$init(self) == FALSE) stop()

      # Assign to object to name  in global environment
      assign(name, self, envir = .GlobalEnv)

      # Log event
      historian$addEvent(className = class(self)[1], objectName = name,
                         method = "initialize",
                         event = private$..stateDesc)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor) {
      visitor$documentXlsx(self)
    }
  )
)
