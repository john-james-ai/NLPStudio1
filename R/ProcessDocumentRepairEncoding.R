#==============================================================================#
#                       ProcessDocumentRepairEncoding                          #
#==============================================================================#
#' ProcessDocumentRepairEncoding
#'
#' \code{ProcessDocumentRepairEncoding} Class responsible for performing encoding repairs in Document objects.
#'
#' Class corrects common encoding errors and converts UTF-8 to ASCII encoding. Replaces
#'
#' @template processClasses.R
#' @template processMethods.R
#' @template processParams.R
#' @template processRepairParams.R
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Repair Family of Classes
#' @export
ProcessDocumentRepairEncoding <- R6::R6Class(
  classname = "ProcessDocumentRepairEncoding",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = ProcessDocument0,

  public = list(

    initialize = function(object, name, substitutions = NULL) {

      private$..className <- "ProcessDocumentRepairEncoding"
      private$..methodName <- "initialize"
      private$..in <- object
      private$..logs <- LogR$new()

      # Validate input
      if (!("Document" %in% class(object))) {
        private$..state <- paste0("Invalid object for this Repair Class.  ",
                                  "This class repairs objects of the Document ",
                                  "class only.  See ?", class(self)[1],
                                  " for further assistance.")
        self$logIt("Error")
        stop()
      }

      if (is.null(substitutions)) {
        private$..substitutions <- NLPStudio:::encodings
      } else {
        private$..substitutions <- substitutions
      }

      # Create new Document object
      private$..out <- Document$new(name = name)
      private$..out <- private$cloneDocument(private$..in, private$..out)

      # log
      private$..state <- paste0("Successfully initialized ProcessDocumentRepairEncoding class object.")
      self$logIt()

      invisible(self)
    },

    process = function() {

      # Obtain content
      content <- private$..out$read()

      # Convert encoding
      Encoding(content) <- "latin1"
      content <- enc2utf8(content)

      # Repair content
      for (i in 1:nrow(private$..substitutions)) {
        content <- gsub(private$..substitutions[[1]][i], private$..substitutions[[2]][i], content)
      }

      # convert UTF-8 to ASCII
      content <- iconv(content, "UTF-8", "ASCII", sub = "")


      # Write content to file
      private$..out$write(content = content)

      # log
      private$..state <- paste0("Successfully performed ProcessDocumentRepairEncoding.")
      self$logIt()

      invisible(self)
    },


    getResult = function() {
      return(private$..out)
    },

    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$processDocumentRepairEncoding(self)
    }
  )
)
