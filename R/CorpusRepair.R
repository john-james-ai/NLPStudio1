#==============================================================================#
#                               CorpusRepair                                   #
#==============================================================================#
#' CorpusRepair
#'
#' \code{CorpusRepair} Class that performs repairs of ASCII control character encodings
#'
#' Class repairs ASCII control character encodings by replacing them with
#' space character. The instantiation method takes a numeric vector containing
#' the decimal representation of the ASCII control characters to replace.
#' The build method constructs the repaired Corpus object with the designated
#' control characters replaced with the space character.
#'
#'
#' @param corpus Object of the Corpus class.
#' @param codes Numeric vector including decimal representations of ASCII control characters to be replaced
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder family of classes
#' @export
CorpusRepair <- R6::R6Class(
  classname = "CorpusRepair",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CorpusSource0,

  private = list(
    ..codes = numeric()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Initialize Method                               #
    #-------------------------------------------------------------------------#
    initialize = function(inCorpus, name, codes) {

      private$..inCorpus <- inCorpus
      private$..name <- name
      private$..codes <- codes

      private$..className <- 'CorpusRepair'
      private$..methodName <- 'initialize'
      private$..state <- "Object of the CorpusRepair class initialized"
      private$..logs <- LogR$new()

      if (!("Corpus" %in% class(inCorpus))) {
        private$..state <- paste0("Unable to repair corpus. The corpus parameter ",
                                  "is not a valid Corpus object.  See ?",
                                  class(self), " for further assistance.")
        self$logIt("Error")
        stop()
      }

      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()

      # Log it
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Repair Method                                 #
    #-------------------------------------------------------------------------#
    build = function() {

      # Create new repaired collection (rc)
      rc <- FileCollection$new(name = name, path = path)

      # Read collection in binary format
      io <- IOBin$new()
      inBin <- collection$read(io)

      # Replace NULL and Substitute control characters with space.
      outBin <- lapply(inBin, function(f) {
          f[f == as.raw(0)] = as.raw(0x20)
        f[f == as.raw(26)] = as.raw(0x20)
      })

      # Create new File objects and add to rc
      files <- collection$getFiles()
      lapply(files, function(f) {
        name <- f$getName()
        fileName <- f$getFileName()
        filePath <- file.path(path, fileName)
        newFile <- File$new(name = name, path = filePath)
        rc$addFile(file = newFile)
      })

      # Write data to new file collection
      rc$write()





      document[document == as.raw(0)] = as.raw(0x20)
      document[document == as.raw(26)] = as.raw(0x20)
      d <- tempfile(fileext = '.txt')
      writeBin(document, d)
      document <- readLines(d)
      unlink(d)

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
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpusRepair(self)
    }
  )
)
