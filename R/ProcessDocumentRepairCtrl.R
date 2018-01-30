#==============================================================================#
#                           ProcessDocumentRepairCtrl                          #
#==============================================================================#
#' ProcessDocumentRepairCtrl
#'
#' \code{ProcessDocumentRepairCtrl} Class responsible for performing repairs of control characters in Document objects.
#'
#' Class repairs control character representations in Document objects.
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
ProcessDocumentRepairCtrl <- R6::R6Class(
  classname = "ProcessDocumentRepairCtrl",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = ProcessDocument0,

  private = list(
    cloneDocument = function(inDocument, outDocument) {

      keys <- names(as.list(inDocument$meta()))
      keys <- keys[keys!= "name"]
      values <- as.list(inDocument$meta())
      values["name"] <- NULL
      outDocument$content <- inDocument$content
      lapply(seq_along(keys), function(k) {
        outDocument$meta(key = keys[[k]], value = values[[k]])
      })

      return(outDocument)
    }
  ),

  public = list(

    initialize = function(object, name, substitutions = NULL) {

      private$..className <- "ProcessDocumentRepairCtrl"
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
        private$..substitutions <- NLPStudio:::ctrl
      } else {
        private$..substitutions <- substitutions
      }


      # Create new Document object
      private$..out <- Document$new(name = name)
      private$..out <- private$cloneDocument(private$..in, private$..out)

      # log
      private$..state <- paste0("Successfully initialized ProcessDocumentRepairCtrl class object.")
      self$logIt()

      invisible(self)
    },

    process = function() {

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
      private$..state <- paste0("Successfully performed ProcessDocumentRepairCtrl.")
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
      visitor$processDocumentRepairCtrl(self)
    }
  )
)
