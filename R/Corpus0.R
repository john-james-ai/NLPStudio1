#==============================================================================#
#                                 Corpus0                                       #
#==============================================================================#
#' Corpus0
#'
#' \code{Corpus0} Class that defines a corpus or collection of documents
#'
#' Class contains a collection of text documents along with document
#' transformations such as NGrams, and POS tagged documents.
#'
#' @section Corpus0 Core Methods:
#'  \describe{
#'   \item{\code{new(name, path)}}{Creates an object of Corpus0 Class}
#'   \item{\code{getName()}}{Returns the name of the Corpus0 object.}
#'   \item{\code{getPath()}}{Returns the path of the Corpus0 object.}
#'  }
#'
#' @section IO Methods:
#'  \describe{
#'   \item{\code{read(io = NULL)}}{Reads a corpus into the Corpus0 object.}
#'   \item{\code{write(io = NULL)}}{Writes a Corpus0 object to file.}
#'  }
#'
#' @section Analysis Methods:
#'  \describe{
#'   \item{\code{stats()}}{Produces a data frame with basic descriptive statistics for the corpus. }
#'   \item{\code{diversity}}{Produces a data frame of lexical diversity measures for the corpus.}
#'   \item{\code{readability}}{Produces a data frame of readability measures for the corpus.}
#'  }
#'
#' @section Meta Data Methods:
#'  \describe{
#'   \item{\code{docMeta(field)}}{Creates a document meta data field.}
#'   \item{\code{corpusMeta(field)}}{Creates a corpus meta data field.}
#'  }
#'
#' @section Other Methods:
#'  \describe{
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#'  }
#'
#' @param field Character string name for a field to be added to the Document or Corpus0 object meta data.
#' @param name A character string containing the name of the Corpus0 object. This variable is used in the instantiation and remove methods.
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Corpus0 <- R6::R6Class(
  classname = "Corpus0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..meta = list(
      simple = list(
        type = character(),
        identifier = character(),
        title = character(),
        language = character(),
        itemType = character(),
        subject = character(),
        created = character(),
        creator = character()
      ),
      dublincore = list(
        type = character(),
        identifier = character(),
        title = character(),
        alternative = character(),
        abstract = character(),
        extent = character(),
        language = character(),
        itemType = character(),
        itemFormat = character(),
        rights = character(),
        accessRights = character(),
        accrualMethod = character(),
        accrualPeriodicity = character(),
        accrualPolicy = character(),
        provenance = character(),
        audience = character(),
        subject = character(),
        spatial = character(),
        temporal = character(),
        created = character(),
        dateItemsCreated = character(),
        creator = character(),
        OWN = character(),
        isLocatedAt = character(),
        isAccessedVia = character(),
        hasPart = character(),
        isPartOf = character(),
        catalogueOrIndex = character(),
        associatedCollection = character(),
        isReferencedBy = character()
      ),
      docMeta = data.frame(),
      docMetaDublin = data.frame()
    ),
    ..documents = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Corpus0 Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(name, path = NULL) { stop("This method is not implemented for this abstract class.") },

    #-------------------------------------------------------------------------#
    #                       Document MetaData Methods                         #
    #-------------------------------------------------------------------------#
    docMeta = function(key = NULL, value) {

      private$..admin$methodName <- 'docMeta'

      if (is.null(key)) {
        key <- names(value)
      }

      if (is.null(key)) {
        key <- paste("docMeta", seq_len(ncol(as.data.frame(value))),
                     sep = "")
      }

      if (length(value) != 1) {
        if (length(value) != length(private$..documents)) {
          private$..state <- paste0("Unable to add document metadata. ",
                                    "The value parameter must be length 1 or ",
                                    "the length equal to the number of documents ",
                                    "in the corpus.  See ?Corpus for further ",
                                    "assistance. ")
          self$logIt("Error")
          stop()
        }
      } else {
        value <- rep(value, length(private$..documents))
      }

      if (length(key) == 1) key <- rep(key, length(value))

      private$..meta$docMeta <- rbindlist(seq_along(value), function(m) {
        private$..documents$docMeta(key = key[[m]], value = value[[m]])
      })

      invisible(private$..meta$docMeta)
    },

    docMetaDublin = function(key, value) {

      private$..admin$methodName <- 'docMetaDublin'

      if (length(value) != 1) {
        if (length(value) != length(private$..documents)) {
          private$..state <- paste0("Unable to add document metadata. ",
                                    "The value parameter must be length 1 or ",
                                    "the length equal to the number of documents ",
                                    "in the corpus.  See ?Corpus for further ",
                                    "assistance. ")
          self$logIt("Error")
          stop()
        }
      } else {
        value <- rep(value, length(private$..documents))
      }

      if (length(key) == 1) key <- rep(key, length(value))

      private$..meta$docMetaDublin <- rbindlist(seq_along(value), function(m) {
        private$..documents$docMetaDublin(key = key[[m]], value = value[[m]])
      })

      invisible(private$..meta$docMetaDublin)
    },

    printDocMeta = function() {
      print(private$..meta$docMeta)
    },

    printDocMetaDublin = function() {
      print(private$..meta$docMetaDublin)
    },

    #-------------------------------------------------------------------------#
    #                         Corpus MetaData Methods                         #
    #-------------------------------------------------------------------------#
    corpusMeta = function(key = NULL, value) {

      private$..admin$methodName <- 'corpusMeta'

      if (is.null(key)) {
        key <- names(value)
      }

      if (is.null(key)) {
        key <- paste("corpusMeta", seq_len(ncol(as.data.frame(value))),
                     sep = "")
      }

      private$..meta$simple[[key]] <- value

      invisible(private$..meta$simple)
    },

    corpusMetaDublin = function(key, value) {

      private$..admin$methodName <- 'corpusMetaDublin'

      if (!key %in% names(private$..meta$dublincore)) {
        private$..admin$state <- paste0("The Dublin Core Metadata structure is immutable. ",
                                        "Use the 'corpusMeta' method to add metadata ",
                                        "variables. See ?Corpus for further assistance. ")
        self$logIt('Error')
        stop()
      }

      private$..meta$dublincore[[key]] <- value

      invisible(private$..meta$dublincore)
    },

    printCorpusMeta = function() {
      df <- as.data.frame(private$..meta$simple)
      names(df) <- Hmisc::capitalize(names(df))
      print(df)
      return(df)
    },

    printCorpusMetaDublin = function() {
      df <- as.data.frame(private$..meta$dublincore)
      names(df) <- Hmisc::capitalize(names(df))
      print(df)
      return(df)
    },



    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        name = private$..name,
        simple = list(
          type = private$..meta$simple$type,
          identifier = private$..meta$simple$identifier,
          title = private$..meta$simple$title,
          language = private$..meta$simple$language,
          itemType = private$..meta$simple$itemType,
          subject = private$..meta$simple$subject,
          created = private$..meta$simple$created,
          creator = private$..meta$simple$creator
        ),
        dublincore = list(
          type = private$..meta$dublincore$type,
          identifier = private$..meta$dublincore$identifier,
          title = private$..meta$dublincore$title,
          alternative = private$..meta$dublincore$alternative,
          abstract = private$..meta$dublincore$abstract,
          extent = private$..meta$dublincore$extent,
          language = private$..meta$dublincore$language,
          itemType = private$..meta$dublincore$itemType,
          itemFormat = private$..meta$dublincore$itemFormat,
          rights = private$..meta$dublincore$rights,
          accessRights = private$..meta$dublincore$accessRights,
          accrualMethod = private$..meta$dublincore$accrualMethod,
          accrualPeriodicity = private$..meta$dublincore$accrualPeriodicity,
          accrualPolicy = private$..meta$dublincore$accrualPolicy,
          provenance = private$..meta$dublincore$provenance,
          audience = private$..meta$dublincore$audience,
          subject = private$..meta$dublincore$subject,
          spatial = private$..meta$dublincore$spatial,
          temporal = private$..meta$dublincore$temporal,
          created = private$..meta$dublincore$created,
          dateItemsCreated = private$..meta$dublincore$dateItemsCreated,
          creator = private$..meta$dublincore$creator,
          OWN = private$..meta$dublincore$OWN,
          isLocatedAt = private$..meta$dublincore$isLocatedAt,
          isAccessedVia = private$..meta$dublincore$isAccessedVia,
          hasPart = private$..meta$dublincore$hasPart,
          isPartOf = private$..meta$dublincore$isPartOf,
          catalogueOrIndex = private$..meta$dublincore$catalogueOrIndex,
          associatedCollection = private$..meta$dublincore$associatedCollection,
          isReferencedBy = private$..meta$dublincore$isReferencedBy
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
        docMeta = private$..meta$docMeta,
        docMetaDublin = private$..meta$docMetaDublin,
        documents = private$..documents
      )
      return(o)
    }
  )
)
