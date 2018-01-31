#==============================================================================#
#                           PreprocessDocumentBin                                 #
#==============================================================================#
#' PreprocessDocumentBin
#'
#' \code{PreprocessDocumentBin} Class responsible for performing repairs of control characters in Document objects.
#'
#' Class repairs control character representations in Document objects.
#'
#' @template preprocessClasses.R
#' @template preprocessMethods.R
#' @template preprocessParams.R
#' @template preprocessRepairParams.R
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family PrePreprocess Family of Classes
#' @export
PreprocessDocumentBin <- R6::R6Class(
  classname = "PreprocessDocumentBin",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = PreprocessDocument0,

  public = list(

    initialize = function(object, name, substitutions = NULL) {

      private$..className <- "PreprocessDocumentBin"
      private$..methodName <- "initialize"
      private$..in <- object
      private$..logs <- LogR$new()

      # Validate input
      if (!("Document" %in% class(object))) {
        private$..state <- paste0("Invalid object for this Preprocess Class.  ",
                                  "This class repairs objects of the Document ",
                                  "class only.  See ?", class(self)[1],
                                  " for further assistance.")
        self$logIt("Error")
        stop()
      }

      if (is.null(substitutions)) {
        private$..substitutions <- NLPStudio:::ctrl
      } else {
        private$..substitutions <- substitutions
      }


      # Create new Document object
      private$..out <- Document$new(name = name)
      private$..out <- private$cloneDocument(private$..in, private$..out)

      # log
      private$..state <- paste0("Successfully initialized PreprocessDocumentBin class object.")
      self$logIt()

      invisible(self)
    },

    preprocess = function() {

      ioBin <- IOBin$new()
      ioText <- IOText$new()

      # Obtain content
      content <- private$..out$read()

      # Save binary data to temp file and re-read
      d <- tempfile(fileext = '.txt')
      writeBin(content, d)
      content <- ioBin$read(path = d)

      # Repair content
      for (i in 1:nrow(private$..substitutions)) {
        content[content == as.raw(private$..substitutions[[1]][i])] = as.raw(private$..substitutions[[2]][i])
      }

      # Write repaired binary data, read, then save as text data
      writeBin(content, d)
      content <- ioBin$read(path = d)
      private$..out$write(content = content)

      # log
      private$..state <- paste0("Successfully performed PreprocessDocumentBin.")
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
      visitor$preprocessDocumentBin(self)
    }
  )
)
