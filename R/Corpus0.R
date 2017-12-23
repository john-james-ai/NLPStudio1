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
    ..content = list(),
    ..meta = list(
      corpus = list(
        simple = list(
          type = private$type,
          identifier = private$identifier,
          title = private$title,
          language = private$language,
          itemType = private$itemType,
          subject = private$subject,
          created = private$created,
          creator = private$creator
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
      document = list(
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
    )
  ),
    ..files = list()
  ),

  active = list(
    # Corpus level Dublin Meta Data Methods
    type = function(value = NULL) return(private$..meta$corpus$dublincore$type <- ifelse(is.null(value),  private$..meta$corpus$dublincore$type,   value)),
    identifier = function(value = NULL) return(private$..meta$corpus$dublincore$identifier <- ifelse(is.null(value),  private$..meta$corpus$dublincore$identifier,   value)),
    title = function(value = NULL) return(private$..meta$corpus$dublincore$title <- ifelse(is.null(value),  private$..meta$corpus$dublincore$title,   value)),
    alternative = function(value = NULL) return(private$..meta$corpus$dublincore$alternative <- ifelse(is.null(value),  private$..meta$corpus$dublincore$alternative,   value)),
    abstract = function(value = NULL) return(private$..meta$corpus$dublincore$abstract <- ifelse(is.null(value),  private$..meta$corpus$dublincore$abstract,   value)),
    extent = function(value = NULL) return(private$..meta$corpus$dublincore$extent <- ifelse(is.null(value),  private$..meta$corpus$dublincore$extent,   value)),
    language = function(value = NULL) return(private$..meta$corpus$dublincore$language <- ifelse(is.null(value),  private$..meta$corpus$dublincore$language,   value)),
    itemType = function(value = NULL) return(private$..meta$corpus$dublincore$itemType <- ifelse(is.null(value),  private$..meta$corpus$dublincore$itemType,   value)),
    itemFormat = function(value = NULL) return(private$..meta$corpus$dublincore$itemFormat <- ifelse(is.null(value),  private$..meta$corpus$dublincore$itemFormat,   value)),
    rights = function(value = NULL) return(private$..meta$corpus$dublincore$rights <- ifelse(is.null(value),  private$..meta$corpus$dublincore$rights,   value)),
    accessRights = function(value = NULL) return(private$..meta$corpus$dublincore$accessRights <- ifelse(is.null(value),  private$..meta$corpus$dublincore$accessRights,   value)),
    accrualMethod = function(value = NULL) return(private$..meta$corpus$dublincore$accrualMethod <- ifelse(is.null(value),  private$..meta$corpus$dublincore$accrualMethod,   value)),
    accrualPeriodicity = function(value = NULL) return(private$..meta$corpus$dublincore$accrualPeriodicity <- ifelse(is.null(value),  private$..meta$corpus$dublincore$accrualPeriodicity,   value)),
    accrualPolicy = function(value = NULL) return(private$..meta$corpus$dublincore$accrualPolicy <- ifelse(is.null(value),  private$..meta$corpus$dublincore$accrualPolicy,   value)),
    provenance = function(value = NULL) return(private$..meta$corpus$dublincore$provenance <- ifelse(is.null(value),  private$..meta$corpus$dublincore$provenance,   value)),
    audience = function(value = NULL) return(private$..meta$corpus$dublincore$audience <- ifelse(is.null(value),  private$..meta$corpus$dublincore$audience,   value)),
    subject = function(value = NULL) return(private$..meta$corpus$dublincore$subject <- ifelse(is.null(value),  private$..meta$corpus$dublincore$subject,   value)),
    spatial = function(value = NULL) return(private$..meta$corpus$dublincore$spatial <- ifelse(is.null(value),  private$..meta$corpus$dublincore$spatial,   value)),
    temporal = function(value = NULL) return(private$..meta$corpus$dublincore$temporal <- ifelse(is.null(value),  private$..meta$corpus$dublincore$temporal,   value)),
    created = function(value = NULL) return(private$..meta$corpus$dublincore$created <- ifelse(is.null(value),  private$..meta$corpus$dublincore$created,   value)),
    dateItemsCreated = function(value = NULL) return(private$..meta$corpus$dublincore$dateItemsCreated <- ifelse(is.null(value),  private$..meta$corpus$dublincore$dateItemsCreated,   value)),
    creator = function(value = NULL) return(private$..meta$corpus$dublincore$creator <- ifelse(is.null(value),  private$..meta$corpus$dublincore$creator,   value)),
    OWN = function(value = NULL) return(private$..meta$corpus$dublincore$OWN <- ifelse(is.null(value),  private$..meta$corpus$dublincore$OWN,   value)),
    isLocatedAt = function(value = NULL) return(private$..meta$corpus$dublincore$isLocatedAt <- ifelse(is.null(value),  private$..meta$corpus$dublincore$isLocatedAt,   value)),
    isAccessedVia = function(value = NULL) return(private$..meta$corpus$dublincore$isAccessedVia <- ifelse(is.null(value),  private$..meta$corpus$dublincore$isAccessedVia,   value)),
    hasPart = function(value = NULL) return(private$..meta$corpus$dublincore$hasPart <- ifelse(is.null(value),  private$..meta$corpus$dublincore$hasPart,   value)),
    isPartOf = function(value = NULL) return(private$..meta$corpus$dublincore$isPartOf <- ifelse(is.null(value),  private$..meta$corpus$dublincore$isPartOf,   value)),
    catalogueOrIndex = function(value = NULL) return(private$..meta$corpus$dublincore$catalogueOrIndex <- ifelse(is.null(value),  private$..meta$corpus$dublincore$catalogueOrIndex,   value)),
    associatedCollection = function(value = NULL) return(private$..meta$corpus$dublincore$associatedCollection <- ifelse(is.null(value),  private$..meta$corpus$dublincore$associatedCollection,   value)),
    isReferencedBy = function(value = NULL) return(private$..meta$corpus$dublincore$isReferencedBy <- ifelse(is.null(value),  private$..meta$corpus$dublincore$isReferencedBy,   value)),

    # Document Level Dublin Metadata Methods
    contributor = function(value = NULL) return(private$..meta$document$dublincore$contributor <- ifelse(is.null(value),  private$..meta$document$dublincore$contributor,   value)),
    coverage = function(value = NULL) return(private$..meta$document$dublincore$coverage <- ifelse(is.null(value),  private$..meta$document$dublincore$coverage,   value)),
    creator = function(value = NULL) return(private$..meta$document$dublincore$creator <- ifelse(is.null(value),  private$..meta$document$dublincore$creator,   value)),
    date = function(value = NULL) return(private$..meta$document$dublincore$date <- ifelse(is.null(value),  private$..meta$document$dublincore$date,   value)),
    description = function(value = NULL) return(private$..meta$document$dublincore$description <- ifelse(is.null(value),  private$..meta$document$dublincore$description,   value)),
    format = function(value = NULL) return(private$..meta$document$dublincore$format <- ifelse(is.null(value),  private$..meta$document$dublincore$format,   value)),
    identifier = function(value = NULL) return(private$..meta$document$dublincore$identifier <- ifelse(is.null(value),  private$..meta$document$dublincore$identifier,   value)),
    language = function(value = NULL) return(private$..meta$document$dublincore$language <- ifelse(is.null(value),  private$..meta$document$dublincore$language,   value)),
    publisher = function(value = NULL) return(private$..meta$document$dublincore$publisher <- ifelse(is.null(value),  private$..meta$document$dublincore$publisher,   value)),
    relation = function(value = NULL) return(private$..meta$document$dublincore$relation <- ifelse(is.null(value),  private$..meta$document$dublincore$relation,   value)),
    rights = function(value = NULL) return(private$..meta$document$dublincore$rights <- ifelse(is.null(value),  private$..meta$document$dublincore$rights,   value)),
    source = function(value = NULL) return(private$..meta$document$dublincore$source <- ifelse(is.null(value),  private$..meta$document$dublincore$source,   value)),
    subject = function(value = NULL) return(private$..meta$document$dublincore$subject <- ifelse(is.null(value),  private$..meta$document$dublincore$subject,   value)),
    title = function(value = NULL) return(private$..meta$document$dublincore$title <- ifelse(is.null(value),  private$..meta$document$dublincore$title,   value)),
    type = function(value = NULL) return(private$..meta$document$dublincore$type <- ifelse(is.null(value),  private$..meta$document$dublincore$type,   value))
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Corpus0 Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(name, path = NULL) { stop("This method is not implemented for this abstract class.") },


    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
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
