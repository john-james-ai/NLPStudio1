#==============================================================================#
#                               Document                                       #
#==============================================================================#
#' Document
#'
#' \code{Document} Class containing the data and methods for corpus documents
#'
#' Defines the object and behavior of the Document object, a leaf component
#' of Corpus composite class.
#'
#' @template documentClasses
#'
#' @section Document methods:
#' \strong{Core Methods:}
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a document. Not implemented for the abstract class.}
#'   \item{\code{getName()}}{Method that returns the name of the Document object. }
#'   \item{\code{setContent(content)}}{Method that sets the content of the Document object. }
#'   \item{\code{read()}}{Method that returns the content of the Document object. }
#'  }
#'
#' \strong{IO Methods:}
#'  \itemize{
#'   \item{\code{read(path, io = NULL)}}{Reads document content from a source designated by the path parameter. }
#'   \item{\code{write(path, io = NULL, content = NULL)}}{Method for writing a document. }
#'  }
#'
#' \strong{Other Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object. Not implemented for this abstract class}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the document.}
#'  }
#'
#' @section Parameters:
#' @param name Character string indicating the file path for a document
#' @param path Character string indicating the path to the docment file.
#' @param parent Corpus object to which the document is composed.
#' @param content List containing character vectors of text.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document classes
#' @export
Document <- R6::R6Class(
  classname = "Document",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..text = character(),
    ..dna = list()  # Data and Analysis list
  ),

  active = list(
    text = function(value) {

      if (missing(value)) {
        private$..meta[['user']] <- Sys.info()["user"]
        private$..meta[['accessed']] <- Sys.time()
        return(private$..text)
      } else {
        private$..text <- value
        private$..meta[['user']] <- Sys.info()["user"]
        private$..meta[["modified"]] <- Sys.time()
        private$..meta[["accessed"]] <- Sys.time()
        private$..state <- "Updated text content."
        self$logIt()
        invisible(self)
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function() {

      # Instantiate variables
      private$..className <- 'Document'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()
      private$..text <- content
      private$..meta[['user']] <- Sys.info()["user"]
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()
      private$..meta[["id"]] <- private$createId()

      if (private$validateParams()$code == FALSE) stop()

      # Create log entry
      private$..state <- paste0("Document id ", private$..meta[["id"]], ", instantiated.")
      self$logIt()

      invisible(self)
    },
    
    #-------------------------------------------------------------------------#
    #                             Data Object Methods                         #
    #-------------------------------------------------------------------------#
    addDNA = function(object) {
      
      private$..methodName <- "addData"
      
      # Validate
      if (!(class(object)[1] %in% c("Data", "Analysis"))) {
        private$..state <- "Objects must be Data or Analysis class objects."
        self$logIt("Error")
        stop()
      }
      
      id <- object$getId()
      private$..dna[[id]] <- object
      private$..state <- paste0("Added ", id, " to Document ", 
                                private$..meta[["name"]], ".")
      self$logIt()
      invisible(self)
    },
    
    getDNA = function(id = NULL, type = NULL) {
      # If id is provided, both meta data and content for the Data or Analyis "DNA"
      # object is returned. Otherwise a data frame containing meta data is returned.
      # If type is provided, the data frame will contain meta data only for the type
      # of DNA object requested.
      
      dna <- rbindlist(lapply(private$..dna, function(d) {
        d$meta()
      }))
      
      if (!is.null(id)) {
        dna <- dna %>% filter(id == id)
      } else if (!is.null(type)) {
        dna <- dna %>% filter(type == type)
      }
      
      return(dna)
      
    },


    #-------------------------------------------------------------------------#
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(path = NULL, io = NULL) {

      private$..methodName <- 'read'

      if (!is.null(path)) {

        private$..meta[["filePath"]] <- path
        private$..meta[["fileName"]] <- basename(path)
      }

      if (!is.null(private$..meta[["filePath"]])) {
        if (is.null(io))  io <- IOFactory$new(private$..meta[["filePath"]])$getIOStrategy()
        private$..text <- io$read(path = private$..meta[["filePath"]])
        private$..state <- paste0("Read ", private$..meta[["name"]], " from ",
                                  private$..meta[["filePath"]], ".")
      }
      self$logIt()
      
      private$..meta[["accessed"]] <- Sys.time()
      
      return(private$..text)
    },

    write = function(path = NULL, io = NULL) {

      private$..methodName <- 'write'

      if (!is.null(path)) {
        private$..meta[["filePath"]] <- path
        private$..meta[["fileName"]] <- basename(path)
      }

      if (is.null(private$..meta[["filePath"]])) {
        private$..state <- paste0("Unable to write document. The file path ",
                                  "parameter must be designated the first ",
                                  "time a file is read from or written to file. ",
                                  "See ?", class(self)[1], " for further assistance.")
        self$logIt("Error")
        stop()
      }

      if (is.null(io))  io <- IOFactory$new(private$..[["filePath"]])$getIOStrategy()

      io$write(path = private$..meta[["filePath"]], content = private$..text)

      private$..state <- paste0("Saved ", private$..meta["name"], " to ", path, ". ")
      self$logIt()
      
      private$..meta[["accessed"]] <- Sys.time()
      invisible(self)
    },
    
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$document(self)
    }
  )
)
