#==============================================================================#
#                               Preprocess                                     #
#==============================================================================#
#' Preprocess
#'
#' \code{Preprocess} Class that performs document preprocessing tasks.
#'
#' Class responsible for document preprocessing tasks,such as encoding,
#' normalization, sanitization, and tokenization of text.
#'
#' @section Document methods:
#'  \itemize{
#'   \item{\code{new()}}{Instantiates an object of the Preprocess class .}
#'   \item{\code{encode()}}{Method for repairing and converting document encoding.}
#'   \item{\code{normalize()}}{Method for normalizing text,e.g. abbrevaitions, common misspellings.}
#'   \item{\code{extract()}}{Method for extracting words or sentences including select words from the text.}
#'  }
#'
#' @param document Object of the DocumentText class.
#' @param malContent Data frame of words to be extracted from the text
#' @param normalizations Data frame of key value pairs of patterns and replacements
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document classes
#' @export
Preprocess <- R6::R6Class(
  classname = "Preprocess",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..className = 'Preprocess',
    ..methodName = character(),
    ..state = character(),
    ..logs = character(),
    ..created = character(),
    ..modified = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$..methodName <- 'initialize'
      private$..state <- "Object of the Preprocess class initialized"
      private$..logs <- LogR$new(NLPStudio$new()$getInstance()$getDirs()$logs)
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                           Encode Method                                 #
    #-------------------------------------------------------------------------#
    encode = function(document) {

      # Read binary format data
      io <- IOBin$new()
      content <- self$read(document, io)

      # Correct encodings
      content[content == as.raw(0)] = as.raw(0x20)
      content[content == as.raw(26)] = as.raw(0x20)
      document$setContent(content)

      # Write binary format data
      self$write(document, io)

      # Logit
      private$..state <- paste0("Binary encoding corrected in ", document$getName, ". ")
      self$logIt()

      # Read and process ASCII conversions
      io <- IOText$new()
      content <- self$read(document, io)

      # Perform encoding changes
      Encoding(content) <- "latin1"
      content <- enc2utf8(content)
      content <- gsub("â€™", "'", content)
      content <- gsub("â€˜", "'", content)
      content <- gsub("â€¦", "", content)
      content <- gsub("â€", "-", content)
      content <- iconv(content, "UTF-8", "ASCII", sub = "")
      document$setContent(content)

      # Write data to disc
      self$write(document, io)

      # Logit
      private$..state <- paste0("ASCII encoding corrected in ", document$getName, ". ")
      self$logIt()

      return(document)
    },

    #-------------------------------------------------------------------------#
    #                      Tokenization and Casing                            #
    #-------------------------------------------------------------------------#
    tokenization = function(document, what = 'sentence') {

      # Read, tokenize into sentences and convert to lower case.
      io <- IOText$new()
      content <- self$read(document, io)

      content <- unlist(parallelizeTask(quanteda::tokenize, content, what = what))

      # Write data
      document$setContent(tolower(content))
      self$write(document, io)

      # Logit
      private$..state <- paste0("Lower case and sentence tokenized ", document$getName, ". ")
      self$logIt()

      return(document)
    },

    #-------------------------------------------------------------------------#
    #                         Normalize Method                                #
    #-------------------------------------------------------------------------#
    normalize = function(document, normalizations = NULL) {

      # Read data
      io <- IOText$new()
      content <- self$read(document, io)

      if (is.null(normalizations)) normalizations <- norms

      key <- paste0("\\b", normalizations$key, "\\b")
      for (i in 1:length(key)) {
        content <- gsub(key[i], normalizations$value[i], content,
                        ignore.case = TRUE, perl = TRUE)
      }

      # Write data
      document$setContent(content)
      self$write(document, io)

      # Logit
      private$..state <- paste0("Normalized text in ", document$getName, ". ")
      self$logIt()

      return(document)
    },

    #-------------------------------------------------------------------------#
    #                           Extract Method                                #
    #-------------------------------------------------------------------------#
    extract = function(document, malContent = NULL, what = 'sentence') {

      # Read data
      io <- IOText$new()
      content <- self$read(document, io)

      if (is.null(malContent)) malContent <- profanity

      if (what == 'sentence') {

        stringsRegex <- paste0("\\b",malContent, "\\b", collapse = '|')
        xidx <- unique(grep(stringsRegex, content, ignore.case = TRUE))
        content <- content[-xidx]
      } else {
        key <- paste0("\\b", malContent$key, "\\b")
        for (i in 1:length(key)) {
          content <- gsub(key[i], " ", content, ignore.case = TRUE, perl = TRUE)
        }
      }

      # Remove extra white-space
      content <- stringr::str_replace(gsub(regexPatterns$whiteSpace, " ",
                                            stringr::str_trim(content)), "B", "b")
      content <- document[content != ""]
      content <- document[content != "'"]

      # Write data
      document$setContent(content)
      self$write(document, io)

      # Logit
      private$..state <- paste0("Extracted malcontent from ", document$getName, ". ")
      self$logIt()

      return(document)
    },

    #-------------------------------------------------------------------------#
    #                              IO Methods                                 #
    #-------------------------------------------------------------------------#
    read = function(document, io = NULL) {

      status <- list()
      status[['code']] <- TRUE

      status <- io$read(document)

      if (status[['code']] == FALSE) {
        status[['code']] <- FALSE
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      } else {
        content <- status[['data']]
      }
      return(content)
    },

    write = function(document, io = NULL) {

      status <- list()
      status[['code']] <- TRUE

      status <- io$write(document)

      if (status[['code']] == FALSE) {
        status[['code']] <- FALSE
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }
    },



    #-------------------------------------------------------------------------#
    #                            Log Method                                   #
    #-------------------------------------------------------------------------#
    logIt = function(level = 'Info', fieldName = NA) {

      private$..logs$entry$owner <- private$..name
      private$..logs$entry$className <- private$..className
      private$..logs$entry$methodName <- private$..methodName
      private$..logs$entry$level <- level
      private$..logs$entry$msg <- private$..state
      private$..logs$entry$fieldName <- fieldName
      private$..logs$created <- Sys.time()
      private$..logs$writeLog()
    }
  )
)
