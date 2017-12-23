#==============================================================================#
#                              DMetaDublin                                      #
#==============================================================================#
#' DMetaDublin
#'
#' \code{DMetaDublin} Class for adding and manipulating Dublincore meta data.
#'
#' DMetadata is an implementation of the Dublin Core DMetadata Initiative DCMI
#' http://dublincore.org/documents/dcmi-terms/#section-3.
#'
#' @section DMetaDublin core methods:
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a document. Not implemented for the abstract class.}
#'   \item{\code{getName()}}{Method that returns the name of the DMetaDublin object. }
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
#' @family DMetaDublin classes
#' @export
DMetaDublin <- R6::R6Class(
  classname = "DMetaDublin",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..contributor = character(),
    ..coverage = character(),
    ..creator = character(),
    ..date = character(),
    ..description = character(),
    ..format = character(),
    ..identifier = character(),
    ..language = character(),
    ..publisher = character(),
    ..relation = character(),
    ..rights = character(),
    ..source = character(),
    ..subject = character(),
    ..title = character(),
    ..type = character()
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$..admin$className <- 'DMetaDublin'
      invisible(self)
    },

    meta = function(key = NULL, value) {

      private$..admin$methodName <- 'meta'

      key <- paste0("..", key)

      if (!key %in% names(private)) {

        private$..state <- paste0("Unable to add metadata to Simple Dublin ",
                                  "Core DMetadata object. User defined metadata ",
                                  "can be defined in the DMetaCustom class.  ",
                                  "See ?DMetaCustom for further assistance.")
        self$logIt()
        stop()
      }
      private[[key]] <- value
      return(private)
    },

    print = function() {
      df <- as.data.frame(private)
      names(df) <- Hmisc::capitalize(substring(names(private), 3))
      print(df)
      return(df)
    }
  )
)
