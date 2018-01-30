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
  inherit = Entity,

  private = list(
    ..ctrl = data.frame(dec = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                                16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31, 127),
                        id = c("NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                               "BS","HT","LF","VT","FF","CR","SO","SI","DLE","DC1",
                               "DC2","DC3","DC4","NAK","SYN","ETB","CAN","EM","SUB",
                               "ESC","FS","GS","RS","US","DEL"),
                        flag = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                 FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                 FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                 FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
                        ),
    setPatterns = function(patterns) {

      if (!is.null(patterns)) {
        private$..ctrl$flag <- FALSE
        if ("numeric" %in% class(patterns)) {
          private$..ctrl$flag[private$..ctrl$dec %in% patterns] <- TRUE
        } else if ("character" %in% class(patterns)) {
          private$..ctrl$flag[private$..ctrl$id %in% patterns] <- TRUE
        } else {
          private$..state <- paste0("Unable to repair ASCII patterns  ", patterns,
                                    ". Patterns must be decimal numeric c(0:31), or ",
                                    private$..ctrl$id, ". See ?", class(self)[1],
                                    " for further assistance.")
          self$logIt("Error")
          stop()
        }
      }
      return(private$..ctrl)
    }
  ),

  public = list(

    initialize = function(object, name, patterns = NULL) {

      private$..className <- "ProcessDocumentRepairCtrl"
      private$..methodName <- "initialize"
      private$..inObject <- object
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
      private$..ctrl <- private$setPatterns(patterns)

      # Create new Document object
      private$..out <- Document$new(name = name)
      private$..out <- private$cloneDocument(private$..in, private$..out)

      # log
      private$..state <- paste0("Successfully initialized ProcessDocumentRepairCtrl class object.")
      self$logIt()

      invisible(self)
    },

    process = function() {

      ioBin <- ioBin$new()
      ioTxt <- ioText$new()

      # Obtain patterns to repair
      patterns <- as.list(subset(private$..ctrl, flag == TRUE, select = dec))

      # Obtain content
      content <- private$..outDocument$read(io = ioBin)

      # Repair content
      for (i in length(patterns)) {
        content[content == as.raw(patterns[[i]])] = as.raw(0x20)
      }

      # Save as binary
      d <- tempfile(fileext = '.txt')
      ioBin$write(path = d, content = content)

      # Reread as text and save
      content <- ioText$read(path = d)
      private$..outDocument$write(content = content)

      # log
      private$..state <- paste0("Successfully performed ProcessDocumentRepairCtrl.")
      self$logIt()

      invisible(self)
    },


    getResult = function() {
      return(private$..outDocument)
    },

    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$processDocumentRepairCtrl(self)
    }
  )
)
