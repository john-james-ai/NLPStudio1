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

  private = list(
    ..content = character(),
    ..meta = list(
      simple = list(
        name = character(),
        author = character(),
        description = character(),
        heading = character(),
        id = character(),
        language = character(),
        origin = character(),
        class = character()
      ),
      dublincore = list(
        contributor = character(),
        coverage = character(),
        creator = character(),
        date = character(),
        description = character(),
        format = character(),
        identifier = character(),
        language = character(),
        publisher = character(),
        relation = character(),
        rights = character(),
        source = character(),
        subject = character(),
        title = character(),
        type = character()
      )
    )
  ),

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
    docMeta = function(key = NULL, value) {

      private$..admin$methodName <- 'meta'

      if (is.null(key)) {
        key <- names(value)
      }

      if (is.null(key)) {
        key <- paste("docMeta", seq_len(ncol(as.data.frame(value))),
                     sep = "")
      }

      private$..meta$simple[[key]] <- value
      return(private$..meta$simple)
    },

    docMetaDublin = function(key = NULL, value) {

      private$..admin$methodName <- 'metaDublin'

      if (is.null(key)) {
        key <- names(value)
      }

      if (!key %in% names(private$..meta$dublincore)) {
        private$..admin$state <- paste0("New metadata variables must be added ",
                                        "using the meta method. ",
                                        "See ?Corpus for further assistance. ")
        self$logIt('Error')
        stop()
      }
      private$..meta$dublincore[[key]] <- value
      return(private$..meta$dublincore)
    },

    printDocMeta = function() {
      df <- as.data.frame(private$..meta$simple)
      names(df) <- Hmisc::capitalize(names(df))
      print(df)
      return(df)
    },

    printDocMetaDublin = function() {
      df <- as.data.frame(private$..meta$dublincore)
      names(df) <- Hmisc::capitalize(names(df))
      print(df)
      return(df)
    },

    #-------------------------------------------------------------------------#
    #                           Expose Object                                 #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        name = private$..name,
        content = private$..content,
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
        file = private$..file
      )
      return(o)
    }
  )
)
