#==============================================================================#
#                              CMetaDublin                                      #
#==============================================================================#
#' CMetaDublin
#'
#' \code{CMetaDublin} Class for adding and manipulating Dublincore meta data.
#'
#' CMetadata is an implementation of the Dublin Core CMetadata Initiative DCMI
#' http://dublincore.org/documents/dcmi-terms/#section-3.
#'
#' @section CMetaDublin core methods:
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a document. Not implemented for the abstract class.}
#'   \item{\code{getName()}}{Method that returns the name of the CMetaDublin object. }
#'   \item{\code{getFileName()}}{Method for obtaining the document file name. .}
#'   \item{\code{getPath()}}{Method for obtaining the document path. }
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
#' @family CMetaDublin classes
#' @export
CMetaDublin <- R6::R6Class(
  classname = "CMetaDublin",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..type = character(),
    ..identifier = character(),
    ..title = character(),
    ..alternative = character(),
    ..abstract = character(),
    ..extent = character(),
    ..language = character(),
    ..itemType = character(),
    ..itemFormat = character(),
    ..rights = character(),
    ..accessRights = character(),
    ..accrualMethod = character(),
    ..accrualPeriodicity = character(),
    ..accrualPolicy = character(),
    ..provenance = character(),
    ..audience = character(),
    ..subject = character(),
    ..spatial = character(),
    ..temporal = character(),
    ..created = character(),
    ..dateItemsCreated = character(),
    ..creator = character(),
    ..OWN = character(),
    ..isLocatedAt = character(),
    ..isAccessedVia = character(),
    ..hasPart = character(),
    ..isPartOf = character(),
    ..catalogueOrIndex = character(),
    ..associatedCollection = character(),
    ..isReferencedBy = character()
  ),

  active = list(
    type = function(value = NULL) return(private$..type   <- ifelse(is.null(value),  private$..type,   value)),
    identifier = function(value = NULL) return(private$..identifier   <- ifelse(is.null(value),  private$..identifier,   value)),
    title = function(value = NULL) return(private$..title   <- ifelse(is.null(value),  private$..title,   value)),
    alternative = function(value = NULL) return(private$..alternative   <- ifelse(is.null(value),  private$..alternative,   value)),
    abstract = function(value = NULL) return(private$..abstract   <- ifelse(is.null(value),  private$..abstract,   value)),
    extent = function(value = NULL) return(private$..extent   <- ifelse(is.null(value),  private$..extent,   value)),
    language = function(value = NULL) return(private$..language   <- ifelse(is.null(value),  private$..language,   value)),
    itemType = function(value = NULL) return(private$..itemType   <- ifelse(is.null(value),  private$..itemType,   value)),
    itemFormat = function(value = NULL) return(private$..itemFormat   <- ifelse(is.null(value),  private$..itemFormat,   value)),
    rights = function(value = NULL) return(private$..rights   <- ifelse(is.null(value),  private$..rights,   value)),
    accessRights = function(value = NULL) return(private$..accessRights   <- ifelse(is.null(value),  private$..accessRights,   value)),
    accrualMethod = function(value = NULL) return(private$..accrualMethod   <- ifelse(is.null(value),  private$..accrualMethod,   value)),
    accrualPeriodicity = function(value = NULL) return(private$..accrualPeriodicity   <- ifelse(is.null(value),  private$..accrualPeriodicity,   value)),
    accrualPolicy = function(value = NULL) return(private$..accrualPolicy   <- ifelse(is.null(value),  private$..accrualPolicy,   value)),
    provenance = function(value = NULL) return(private$..provenance   <- ifelse(is.null(value),  private$..provenance,   value)),
    audience = function(value = NULL) return(private$..audience   <- ifelse(is.null(value),  private$..audience,   value)),
    subject = function(value = NULL) return(private$..subject   <- ifelse(is.null(value),  private$..subject,   value)),
    spatial = function(value = NULL) return(private$..spatial   <- ifelse(is.null(value),  private$..spatial,   value)),
    temporal = function(value = NULL) return(private$..temporal   <- ifelse(is.null(value),  private$..temporal,   value)),
    created = function(value = NULL) return(private$..created   <- ifelse(is.null(value),  private$..created,   value)),
    dateItemsCreated = function(value = NULL) return(private$..dateItemsCreated   <- ifelse(is.null(value),  private$..dateItemsCreated,   value)),
    creator = function(value = NULL) return(private$..creator   <- ifelse(is.null(value),  private$..creator,   value)),
    OWN = function(value = NULL) return(private$..OWN   <- ifelse(is.null(value),  private$..OWN,   value)),
    isLocatedAt = function(value = NULL) return(private$..isLocatedAt   <- ifelse(is.null(value),  private$..isLocatedAt,   value)),
    isAccessedVia = function(value = NULL) return(private$..isAccessedVia   <- ifelse(is.null(value),  private$..isAccessedVia,   value)),
    hasPart = function(value = NULL) return(private$..hasPart   <- ifelse(is.null(value),  private$..hasPart,   value)),
    isPartOf = function(value = NULL) return(private$..isPartOf   <- ifelse(is.null(value),  private$..isPartOf,   value)),
    catalogueOrIndex = function(value = NULL) return(private$..catalogueOrIndex   <- ifelse(is.null(value),  private$..catalogueOrIndex,   value)),
    associatedCollection = function(value = NULL) return(private$..associatedCollection   <- ifelse(is.null(value),  private$..associatedCollection,   value)),
    isReferencedBy = function(value = NULL) return(private$..isReferencedBy   <- ifelse(is.null(value),  private$..isReferencedBy,   value))
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         CMetaDublin Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(name, path = NULL) {

      private$..admin$className <- 'CMetaDublin'
      private$..admin$methodName <- 'initialize'
      invisible(self)
    },


    meta = function(key = NULL, value) {

      private$..admin$methodName <- 'meta'

      key <- paste0("..", key)

      if (!key %in% names(private)) {

        private$..admin$state <- paste0("Unable to add metadata to Dublin ",
                                  "Core CMetadata object. User defined metadata ",
                                  "can be defined in the DMetaSimple class.  ",
                                  "See ?DMetaSimple for further assistance.")
        self$logIt()
        stop()
      }
      private[[key]] <- value
    },

    print = function() {
      df <- as.data.frame(private)
      names(df) <- Hmisc::capitalize(substring(names(private), 3))
      print(df)
      df
    }
  )
)
