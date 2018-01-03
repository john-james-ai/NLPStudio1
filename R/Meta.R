#==============================================================================#
#                                 Meta                                         #
#==============================================================================#
#' Meta
#'
#' \code{Meta} Class defines data and methods for managing metadata for Corpus and Document objects.
#'
#' The metadata structures defined for Corpus and Document objects are based
#' upon the Dublin Core Metadata Initiative http://dublincore.org/documents/dcmi-terms/.
#' Four types of metadata are defined for Corpus and Document objects:
#' \itemize{
#'  \item CorpusMetaDublin DublinCoreCollections Dublin Core Metadata Initiative terms for document collections http://dublincore.org/groups/collections/collection-application-profile/
#'  \item DocumentMetaDublin Dublin Core Metadata Initiative terms for documents http://dublincore.org/documents/dcmi-terms/
#'  \item CorpusMeta An abbreviated list including eight terms from the Dublin document collection terms
#'  \item DocumentMeta An abbreviated list including six terms from the Dublin Core Metadata Element Set
#' }
#'
#' @section Methods:
#'  \describe{
#'   \item{\code{new()}}{Not implemented for this class}
#'   \item{\code{getCorpusMeta()}}{Returns a nested list of the simplified and DCMI corpus metadata variables. }
#'   \item{\code{getDocumentMeta()}}{Returns a nested list of the simplified and DCMI document metadata variables. }
#'  }
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Meta <- R6::R6Class(
  classname = "Meta",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..meta = list(
      corpus = list(
        title = NULL,
        alternative = NULL,
        subject = NULL,
        type = NULL,
        identifier = NULL,
        creator = NULL,
        created = NULL,
        OWN = NULL,
        itemType = NULL,
        itemFormat = NULL,
        dateItemsCreated = NULL,
        language = NULL,
        abstract = NULL,
        audience = NULL,
        extent = NULL,
        provenance = NULL,
        spatial = NULL,
        temporal = NULL,
        rights = NULL,
        accessRights = NULL,
        accrualMethod = NULL,
        accrualPeriodicity = NULL,
        accrualPolicy = NULL,
        isAccessedVia = NULL,
        isLocatedAt = NULL,
        isPartOf = NULL,
        isReferencedBy = NULL,
        associatedCollection = NULL,
        hasPart = NULL,
        catalogueOrIndex = NULL
      ),
      document = list(
        title  = NULL,
        subject  = NULL,
        description  = NULL,
        type  = NULL,
        creator  = NULL,
        date  = NULL,
        contributor  = NULL,
        source  = NULL,
        language  = NULL,
        format  = NULL,
        identifier  = NULL,
        coverage  = NULL,
        publisher  = NULL,
        relation  = NULL,
        rights  = NULL
      )
    )
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Meta Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function() { stop("This method is not implemented for this abstract class.") }
  )
)
