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
#' @section Other method:
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object.}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the document.}
#'  }
#'
#'
#' @param filePath Character string indicating the file path for a document
#' @param parent Object of the Corpus or Set classes to which this document belongs
#' @param io An object of one of the IO classes used for reading and writing.
#' @param norms A list of key value pairs of strings to be replaced in the normalization step
#' @param profanity Character vector of profane words to be normalized or extracted from the text
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document classes
#' @export
Document <- R6::R6Class(
  classname = "Document",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..io = character(),
    ..content = character(),
    ..size = numeric()
  ),

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

    getFileName = function() private$..fileName,
    getContent = function() {
      self$read()
      private$..content
    },

    #-------------------------------------------------------------------------#
    #                            IO Methods                                   #
    #-------------------------------------------------------------------------#
    cloneContent = function(document) {
      private$..content <- document$getContent()
    },

    read = function() {

      private$..methodName <- 'read'

      status <- private$..io()$read(self)

      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      } else {
        private$..content <- status[['data']]
      }

      # LogIt
      private$..state <- paste0("Read ", private$..fileName, ". ")
      private$..accessed <- Sys.time()
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)

    },

    write = function() {

      private$..methodName <- 'write'

      status <- private$..io()$write(self)

      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # LogIt
      private$..state <- paste0("Wrote ", private$..fileName, ". ")
      private$..modified <- Sys.time()
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                         Aggregate Methods                               #
    #-------------------------------------------------------------------------#
    move = function(parent) {

      private$..methodName <- 'move'

      v <- Validator$new()
      status <- v$setParent(self, parent)

      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      } else {
        private$..parent <- parent

        # Move File
        from <- private$..path
        to <- file.path(private$..parent$getPath(), 'documents/text', private$..fileName)
        f <- FileManager$new()
        status <- f$moveFile(from, to)
        if (status[['code']] == FALSE) {
          private$..state <- status[['msg']]
          self$logIt(level = 'Error')
          stop()
        }

        private$..path <- to
        private$..modified <- Sys.time()
        private$..state <- paste(private$..className, private$..name, 'moved to ',
                                 parent$getClassName(), parent$getName())
        self$logIt()

        # Assign its name in the global environment
        assign(private$..name, self, envir = .GlobalEnv)

        invisible(self)
      }
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
      self$write(io)

      # Logit
      private$..modified <- Sys.time()
      private$..state <- paste("Repaired/refined", private$..name, "document.")
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Start Processing                              #
    #-------------------------------------------------------------------------#
    start = function() {
      io <- IORdata$new()
      self$read()
    },


    #-------------------------------------------------------------------------#
    #                   Reshape into (lower case) Sentences                   #
    #-------------------------------------------------------------------------#
    reshapeSent = function(what = 'sentence', lower = TRUE) {

      private$..methodName <- 'tokenize'

      private$..content <- unlist(parallelizeTask(quanteda::tokenize,
                                                  private$..content,
                                                  what = what))
      if (lower == TRUE) private$..content <- tolower(private$..content)
    },


    #-------------------------------------------------------------------------#
    #                       Text Parsing Methods                              #
    #-------------------------------------------------------------------------#
    parseFast = function(what = 'sentence', numbers = FALSE, punct = FALSE,
                         symbols = FALSE,  twitter = FALSE, hyphens = FALSE,
                         url = FALSE) {
      private$..content <- unlist(parallelizeTask(quanteda::tokenize,
                                                  private$..content,
                                                  remove_numbers = numbers,
                                                  remove_punct = punct,
                                                  remove_symbols = symbols,
                                                  remove_twitter = twitter,
                                                  remove_hyphens = hyphens,
                                                  remove_url = url,
                                                  what = what))

    },

    parseEmail = function() {private$..content <- gsub(regexPatterns$emails, ' ',
                                                       private$..content, perl = TRUE)},

    parseControl = function() {private$..content <- gsub(regexPatterns$control, ' ',
                                                         private$..content, perl = TRUE)},

    parseRepeatChars = function() {private$..content <- gsub(regexPatterns$repeatedChars, ' ',
                                                             private$..content, perl = TRUE)},

    parseLongWords = function() {private$..content <- gsub(regexPatterns$longWords, ' ',
                                                           private$..content, perl = TRUE)},

    #-------------------------------------------------------------------------#
    #                     Text Normalization Methods                          #
    #-------------------------------------------------------------------------#
    normalize = function() {

      key <- paste0("\\b", norms$key, "\\b")
      for (i in 1:length(key)) {
        private$..content <- gsub(key[i], norms$value[i], private$..content,
                        ignore.case = TRUE, perl = TRUE)
      }
      # Clean up
      self$cleanUp()
    },

    #-------------------------------------------------------------------------#
    #                     Text Sanitazation Methods                           #
    #-------------------------------------------------------------------------#
    sanitizeWord = function() {

      key <- paste0("\\b", profanity, "\\b")
      for (i in 1:length(key)) {
        private$..content <- gsub(key[i], ' ', private$..content,
                                  ignore.case = TRUE, perl = TRUE)
      }
    },

    sanitizeSent = function() {

      stringsRegex <- paste0("\\b",profanity, "\\b", collapse = '|')
      xidx <- unique(grep(stringsRegex, private$..content, ignore.case = TRUE))
      private$..content <- private$..content[-xidx]
    },

    sanitizeTag = function() {

      key <- paste0("\\b", profanity, "\\b")
      for (i in 1:length(key)) {
        private$..content <- gsub(key[i], '<EXPLETIVE>', private$..content,
                                  ignore.case = TRUE, perl = TRUE)
      }
    },

    #-------------------------------------------------------------------------#
    #                          Clean Up Method                                #
    #-------------------------------------------------------------------------#
    cleanUp = function() {

      # Remove extra white-space
      private$..content <-
        stringr::str_replace(gsub(regexPatterns$whiteSpace, " ",
                                  stringr::str_trim(private$..content)), "B", "b")
      private$..content <- private$..content[private$..content != ""]
      private$..content <- private$..content[private$..content != "'"]
    },

    #-------------------------------------------------------------------------#
    #                          Complete Processing                            #
    #-------------------------------------------------------------------------#
    complete = function() {
      io <- IORdata$new()
      self$write(io)
      private$..content <- NULL
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
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$documentText(self)
    },

    #-------------------------------------------------------------------------#
    #                           Expose Object                                 #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        className	 =  private$..className ,
        name	 = 	    private$..name ,
        fileName	 =  private$..fileName ,
        desc	 = 	    private$..desc ,
        parent	 = 	  private$..parent ,
        path	 = 	    private$..path ,
        content =     private$..content,
        state	 = 	    private$..state ,
        logs	 = 	    private$..logs ,
        size	 = 	    private$..size ,
        modified	 = 	private$..modified ,
        created	 = 	  private$..created ,
        accessed	 = 	private$..accessed
      )
      return(o)
    }

  )
)
