#==============================================================================#
#                               Document0                                      #
#==============================================================================#
#' Document0
#'
#' \code{Document0} Abstract class for the Document family of classes
#'
#' Defines the structure, identity and core behaviors for the Document
#' classes.
#'
#' @section Document0 core methods:
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a document. Not implemented for the abstract class.}
#'   \item{\code{getName()}}{Method that returns the name of the Document0 object. }
#'   \item{\code{getFileName()}}{Method for obtaining the document file name. .}
#'   \item{\code{getPath()}}{Method for obtaining the document path. }
#'  }
#'
#' @section Document0 getter/setter methods:
#'  \itemize{
#'   \item{\code{desc()}}{Method for setting or retrieving the Document0 object description.}
#'  }
#'
#'  @section Document0 IO methods:
#'  \itemize{
#'   \item{\code{getContent()}}{Method for obtaining the document content.}
#'   \item{\code{addContent(content)}}{Method for adding content to a document.}
#'   \item{\code{read(io)}}{Method for reading a document. }
#'   \item{\code{write(io)}}{Method for writing a document. }
#'  }
#'
#' @section Document0 aggregation method:
#'  \itemize{
#'   \item{\code{setParent(parent)}}{Sets the parent Corpus object. }
#'  }
#'
#'
#' @section Other methods:
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object. Not implemented for this abstract class}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the document.}
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
#' @family Document0 classes
#' @export
Document0 <- R6::R6Class(
  classname = "Document0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Corpus0,

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
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name, file = NULL) { stop("This method is not implemented for this abstract class.") },

    #-------------------------------------------------------------------------#
    #                         Meta Data Methods                               #
    #-------------------------------------------------------------------------#
    meta = function(key = NULL, value) {

      private$..admin$methodName <- 'metaSimple'

      if (is.null(key)) {
        key <- names(value)
      }

      if (!key %in% names(private$..meta$simple)) {
        private$..admin$state <- paste0("Not able to add meta data variables ",
                                        "at the document level. New variables ",
                                        "must be added at the corpus level. ",
                                        "See ?Corpus for further assistance. ")
        self$logIt('Error')
        stop()
      }
      private$..meta$simple[[key]] <- value
    },

    metaDublin = function(key = NULL, value) {

      private$..admin$methodName <- 'metaDublin'

      if (is.null(key)) {
        key <- names(value)
      }

      if (!key %in% names(private$..meta$dublin)) {
        private$..admin$state <- paste0("New metadata variables must be added ",
                                        "using the meta method. ",
                                        "See ?Corpus for further assistance. ")
        self$logIt('Error')
        stop()
      }
      private$..meta$dublin[[key]] <- value
    },

    printMeta = function() {
      df <- as.data.frame(private$..meta$simple)
      names(df) <- Hmisc::capitalize(names(df))
      print(df)
      return(df)
    },

    printMetaDublin = function() {
      df <- as.data.frame(private$..meta$dublin)
      names(df) <- Hmisc::capitalize(names(df))
      print(df)
      return(df)
    },

    #-------------------------------------------------------------------------#
    #                           Expose Object                                 #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        simple = list(
          name = private$..meta$simple$name,
          author = private$..meta$simple$author,
          description = private$..meta$simple$description,
          heading = private$..meta$simple$heading,
          id = private$..meta$simple$id,
          language = private$..meta$simple$language,
          origin = private$..meta$simple$origin,
          class = private$..meta$simple$class
        ),
        admin = list(
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
          contributor = private$..meta$dublin$contributor,
          coverage = private$..meta$dublin$coverage,
          creator = private$..meta$dublin$creator,
          date = private$..meta$dublin$date,
          description = private$..meta$dublin$description,
          format = private$..meta$dublin$format,
          identifier = private$..meta$dublin$identifier,
          language = private$..meta$dublin$language,
          publisher = private$..meta$dublin$publisher,
          relation = private$..meta$dublin$relation,
          rights = private$..meta$dublin$rights,
          source = private$..meta$dublin$source,
          subject = private$..meta$dublin$subject,
          title = private$..meta$dublin$title,
          type = private$..meta$dublin$type
          ),
        content = private$..content,
        file = private$..file
        )
      return(o)
    }
  )
)
