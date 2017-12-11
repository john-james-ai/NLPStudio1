#==============================================================================#
#                               Document                                       #
#==============================================================================#
#' Document
#'
#' \code{Document} Class that contains document meta data and text content.
#'
#' This class contains the data and methods for creating, reading, manipulating,
#' and processing text documents.
#'
#' @section Document core methods:
#'  \itemize{
#'   \item{\code{new(filePath, desc = NULL)}}{Method for instantiating a document.}
#'   \item{\code{getName()}}{Method that returns the name of the Document object.}
#'   \item{\code{getFileName()}}{Method for obtaining the document file name.}
#'   \item{\code{getPath()}}{Method for obtaining the document path.}
#'   \item{\code{getContent()}}{Method for obtaining the document content.}
#'  }
#'
#' @section Document getter/setter methods:
#'  \itemize{
#'   \item{\code{desc()}}{Method for setting or retrieving the Document object description.}
#'  }
#'
#'  @section Document IO methods:
#'  \itemize{
#'   \item{\code{addContent(content)}}{Method for adding content to a document.}
#'   \item{\code{read(io)}}{Method for reading a document.}
#'   \item{\code{write(io)}}{Method for writing a document.}
#'  }
#'
#' @section Document aggregation method:
#'  \itemize{
#'   \item{\code{move(parent)}}{Moves a document to the designated parent corpus.}
#'  }
#'
#'  @section Document Processing Methods:
#'  \itemize{
#'   \item{\code{refine()}}{Repairs encoding and format errors in text document.}
#'   \item{\code{reshapeSent()}}{Reshapes the document into sentences.}
#'   \item{\code{parseFast()}}{Relatively fast text parser.}
#'   \item{\code{parseEmail()}}{Parses email addresses from the document.}
#'   \item{\code{parseControl()}}{Parses control characters from the document.}
#'   \item{\code{parseRepeatChars()}}{Parses repeated characters from the document.}
#'   \item{\code{parseLongWords()}}{Parses words > 40 chars, from the document.}
#'   \item{\code{normalize()}}{Normalizes abbreviations and contractions in a document.}
#'   \item{\code{sanitizeWord()}}{Removes profane words from a document.}
#'   \item{\code{sanitizeSent()}}{Removes sentences containing profane words from a document.}
#'   \item{\code{sanitizeTag()}}{Replaces profane words with the <EXPLETIVE> tag.}
#'   \item{\code{cleanUp()}}{Removes extra white-space, stray hyphens an empty sentences.}
#'  }
#'
#' @section Other methods:
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object.}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the document.}
#'  }
#'
#'
#' @param filePath Character string indicating the file path for a document
#' @param parent Object of the Corpus or Set classes to which this document belongs
#' @param io An object of one of the IO classes used for reading and writing.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document classes
#' @export
Document <- R6::R6Class(
  classname = "Document",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Document0,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(filePath, io, desc = NULL) {

      private$..methodName <- 'initialize'

      # Instantiate variables
      private$..className <- 'Document'
      private$..name <- tools::file_path_sans_ext(basename(filePath))
      private$..fileName <- basename(filePath)
      private$..desc <- ifelse(is.null(desc), private$..fileName, desc)
      private$..parent <- NULL
      private$..path <- filePath
      private$..io <- io
      private$..content <- NULL
      private$..state <- paste("Document", private$..name, "instantiated at", Sys.time())
      private$..logs <- LogR$new()
      private$..size <- file.info(filePath)$size
      private$..modified <- file.info(filePath)$mtime
      private$..created <- file.info(filePath)$ctime
      private$..accessed <- file.info(filePath)$atime

      # Validate Document
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }
      # Create log entry
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                     Refinement / Repair Method                          #
    #-------------------------------------------------------------------------#
    refine = function() {

      private$..methodName <- 'refine'

      # Repair File
      io <- IOBin$new()
      self$read(io)
      content <- private$..content
      content[content == as.raw(0)] = as.raw(0x20)
      content[content == as.raw(26)] = as.raw(0x20)
      private$..content <-  content
      self$write(io)

      # Remove non-ASCII quotes and hyphens
      io <- IOText$new()
      self$read(io)
      content <- private$..content
      Encoding(content) <- "latin1"
      content <- enc2utf8(content)
      content <- gsub("â€™", "'", content)
      content <- gsub("â€˜", "'", content)
      content <- gsub("â€¦", "", content)
      content <- gsub("â€", "-", content)
      content <- iconv(content, "UTF-8", "ASCII", sub = "")
      private$..content <- content

      # Reshape into sentences
      private$..content <- parallelizeTask(quanteda::tokens,
                                           private$..content,
                                           what = 'sentence')

      # Convert to lower case
      private$..content <- tolower(private$..content)

      # Rename file, change IO class, and write file in new rdata format
      private$..fileName <- paste0(private$..name, '.Rdata')
      private$..path <- file.path(dirname(private$..path), private$..fileName)
      private$..io <- IORdata$new()
      self$write()

      # Logit
      private$..modified <- Sys.time()
      private$..state <- paste("Refined", private$..name, "document.")
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Text Parsing Methods                              #
    #-------------------------------------------------------------------------#
    parseFast = function(numbers = FALSE, punct = FALSE, symbols = FALSE,
                         twitter = FALSE, hyphens = FALSE, url = FALSE) {

      private$..methodName <- 'parseFast'

      private$..content <- parallelizeTask(quanteda::tokens,
                                           private$..content,
                                           remove_numbers = numbers,
                                           remove_punct = punct,
                                           remove_symbols = symbols,
                                           remove_twitter = twitter,
                                           remove_hyphens = hyphens,
                                           remove_url = url)


      # Logit
      private$..modified <- Sys.time()
      private$..state <- paste("Parsed", private$..name, "using parseFast method.")
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    parseFull = function(numbers = FALSE, punct = FALSE,
                         symbols = FALSE,  twitter = FALSE, hyphens = FALSE,
                         url = FALSE, email = FALSE, control = FALSE,
                         repeatChars = FALSE, longWords = FALSE) {

      private$..methodName <- 'parseFull'

      private$..content <- parallelizeTask(quanteda::tokens,
                                           private$..content,
                                           remove_numbers = numbers,
                                           remove_punct = punct,
                                           remove_symbols = symbols,
                                           remove_twitter = twitter,
                                           remove_hyphens = hyphens,
                                           remove_url = url)


      if (email == TRUE) {
        private$..content <- lapply(private$..content, function(x) {
          gsub(NLPStudios:::regexPatterns$emails, ' ', x, perl = TRUE)
        })
      }

      if (control == TRUE) {
        private$..content <- lapply(private$..content, function(x) {
          gsub(NLPStudios:::regexPatterns$control, ' ', x, perl = TRUE)
        })
      }

      if (repeatChars == TRUE) {
        private$..content <- lapply(private$..content, function(x) {
          gsub(NLPStudios:::regexPatterns$repeatedChars, ' ', x, perl = TRUE)
        })
      }

      if (longWords == TRUE) {
        private$..content <- lapply(private$..content, function(x) {
          gsub(NLPStudios:::regexPatterns$longWords, ' ', x, perl = TRUE)
        })
      }

      # Logit
      private$..modified <- Sys.time()
      private$..state <- paste("Parsed", private$..name, "using parseFull method.")
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                     Text Normalization Methods                          #
    #-------------------------------------------------------------------------#
    normalize = function() {

      private$..methodName <- 'normalize'

      key <- paste0("\\b", NLPStudios:::norms$key, "\\b")
      for (i in 1:length(key)) {
        private$..content <- gsub(key[i], NLPStudios::norms$value[i],
                                  private$..content,
                                  ignore.case = TRUE, perl = TRUE)
      }

      # Logit
      private$..modified <- Sys.time()
      private$..state <- paste0("Normalized ", private$..name, ".")
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                     Text Sanitization Methods                           #
    #-------------------------------------------------------------------------#
    sanitizeWord = function() {

      private$..methodName <- 'sanitizeWord'

      key <- paste0("\\b", NLPStudios:::profanity, "\\b")
      for (i in 1:length(key)) {
        private$..content <- gsub(key[i], ' ', private$..content,
                                  ignore.case = TRUE, perl = TRUE)
      }

      # Logit
      private$..modified <- Sys.time()
      private$..state <- paste("Sanitized", private$..name, "using sanitizeWord method.")
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    sanitizeSent = function() {

      private$..methodName <- 'sanitizeSent'

      stringsRegex <- paste0("\\b",NLPStudios:::profanity, "\\b", collapse = '|')
      xidx <- unique(grep(stringsRegex, private$..content, ignore.case = TRUE))
      private$..content <- private$..content[-xidx]

      # Logit
      private$..modified <- Sys.time()
      private$..state <- paste("Sanitized", private$..name, "using sanitizeSent method.")
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    sanitizeTag = function() {

      private$..methodName <- 'sanitizeTag'

      key <- paste0("\\b", NLPStudios:::profanity, "\\b")
      for (i in 1:length(key)) {
        private$..content <- gsub(key[i], '<EXPLETIVE>', private$..content,
                                  ignore.case = TRUE, perl = TRUE)
      }

      # Logit
      private$..modified <- Sys.time()
      private$..state <- paste("Sanitized", private$..name, "using sanitizeTag method.")
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Commit Changes                                #
    #-------------------------------------------------------------------------#
    commit = function() {

      private$..methodName <- 'commit'

      # Remove extra white-space
      private$..content <-
        stringr::str_replace(gsub(NLPStudios:::regexPatterns$whiteSpace, " ",
                                  stringr::str_trim(private$..content)), "B", "b")
      private$..content <- private$..content[private$..content != ""]
      private$..content <- private$..content[private$..content != "'"]

      # Logit
      private$..modified <- Sys.time()
      private$..state <- paste("Commited preprocessing of", private$..name, "at", Sys.time())
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      # Write to file
      self$write()

      # Clear memory
      private$..content <- NULL

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$document(self)
    }
  )
)
