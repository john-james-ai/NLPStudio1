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
#' @param document Object of the Document class.
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
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                           Encode Method                                 #
    #-------------------------------------------------------------------------#
    encode = function(content) {

      # Perform encoding changes
      Encoding(content) <- "latin1"
      content <- enc2utf8(content)
      content <- gsub("â€™", "'", content)
      content <- gsub("â€˜", "'", content)
      content <- gsub("â€¦", "", content)
      content <- gsub("â€", "-", content)
      content <- iconv(content, "UTF-8", "ASCII", sub = "")

      return(content)
    },


    #-------------------------------------------------------------------------#
    #                     Tokenization and Lower Casing                       #
    #-------------------------------------------------------------------------#
    tokenize = function(content, what = 'sentence', lower = TRUE) {

      content <- unlist(parallelizeTask(quanteda::tokenize, content, what = what))
      if (lower == TRUE) content <- tolower(content)

      return(content)
    },

    #-------------------------------------------------------------------------#
    #                                Parsing                                  #
    #-------------------------------------------------------------------------#
    parse = function(content, emails = TRUE, urls = TRUE, twitter = TRUE,
                     controls = TRUE, punct = TRUE,  hyphens = TRUE,
                     symbols = TRUE, apostrophe = FALSE,  digits = TRUE,
                     repeatChars = TRUE, longWords = TRUE) {

      if (emails == TRUE) content <- gsub(regexPatterns$emails, ' ', content, perl = TRUE)
      if (urls == TRUE)   content <- gsub(regexPatterns$urls, ' ', content, perl = TRUE)
      if (twitter == TRUE) content <- gsub(regexPatterns$twitter, ' ', content, perl = TRUE)
      if (controls == TRUE) content <- gsub(regexPatterns$control, ' ', content, perl = TRUE)
      if (hyphens == TRUE) content <- gsub(regexPatterns$hyphens, ' ', content, perl = TRUE)
      if (apostrophe == TRUE) content <- gsub(regexPatterns$apostrophe, ' ', content, perl = TRUE)
      if (punct == TRUE) content <- gsub(regexPatterns$punctSansApos, ' ', content, perl = TRUE)
      if (symbols == TRUE) content <- gsub(regexPatterns$symbols, ' ', content, perl = TRUE)
      if (digits == TRUE) content <- gsub(regexPatterns$digits, ' ', content, perl = TRUE)
      if (repeatChars == TRUE) content <- gsub(regexPatterns$repeatedChars, '\\2', content, perl = TRUE)
      if (longWords == TRUE) content <- gsub(regexPatterns$longWords, '', content, perl = TRUE)

      # Cleanup
      content <- content[content != ""]
      content <- content[content != "'"]
      content <- str_replace(gsub(regexPatterns$whiteSpace, " ", str_trim(content)), "B", "b")


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
