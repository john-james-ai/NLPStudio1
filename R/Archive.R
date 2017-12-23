#==============================================================================#
#                               Archive                                        #
#==============================================================================#
#' Archive
#'
#' \code{Archive} Class responsible for archival and restore of NLPStudio objects.
#'
#' Class archives NLPStudio objects, including Studio, Data, Corpus & Document
#' objects. A list of archives is retained and a restoral method enables clients
#' to restore objects the object to its prior state.
#'
#' @section Archive core methods:
#'  \itemize{
#'   \item{\code{new(name, object, desc = NULL)}}{Method for instantiating a Archive.}
#'   \item{\code{getArchives()}}{Method that returns the name of the Archive object.}
#'   \item{\code{archive()}}{Method for obtaining the Archive file name.}
#'   \item{\code{restore(archive)}}{Method for obtaining the Archive path.}
#'  }
#'
#' @section Archive getter/setter methods:
#'  \itemize{
#'   \item{\code{desc()}}{Method for setting or retrieving the Archive object description.}
#'  }
#'
#' @section Other methods:
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object.}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the Archive.}
#'  }
#'
#'
#' @param object Object to archived
#' @param name Character string containing the name of the archive
#' @param desc Character string containing the description of the archive
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Archive classes
#' @export
Archive <- R6::R6Class(
  classname = "Archive",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name, object, desc = NULL) {

      private$..admin$methodName <- 'initialize'

      # Instantiate variables
      private$..admin$className <- 'Archive'
      private$..admin$name <- name
      private$..desc <- ifelse(is.null(desc), private$..fileName, desc)
      private$..parent <- NLPStudio$new()$getInstance()
      private$..homeDir <- file.path(private$..parent()$getPath(), 'archive')
      private$..admin$path <- file.path(private$..homeDir, name)
      private$..admin$state <- paste("Archive", private$..admin$name, "instantiated at", Sys.time())
      private$..admin$logs <- LogR$new()

      # Validate Archive
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..admin$state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }
      # Create log entry
      self$logIt()

      # Assign its name in the global environment
      assign(private$..admin$name, self, envir = .GlobalEnv)

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                        Archive / Restore Methods                        #
    #-------------------------------------------------------------------------#
    archive = function() {

      private$..admin$methodName <- 'archive'


      # Logit
      private$..admin$modified <- Sys.time()
      private$..admin$state <- paste("Refined", private$..admin$name, "Archive.")
      self$logIt()

      # Assign its name in the global environment
      assign(private$..admin$name, self, envir = .GlobalEnv)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Text Parsing Methods                              #
    #-------------------------------------------------------------------------#
    parseFast = function(numbers = FALSE, punct = FALSE, symbols = FALSE,
                         twitter = FALSE, hyphens = FALSE, url = FALSE) {

      private$..admin$methodName <- 'parseFast'

      private$..content <- parallelizeTask(quanteda::tokens,
                                           private$..content,
                                           remove_numbers = numbers,
                                           remove_punct = punct,
                                           remove_symbols = symbols,
                                           remove_twitter = twitter,
                                           remove_hyphens = hyphens,
                                           remove_url = url)


      # Logit
      private$..admin$modified <- Sys.time()
      private$..admin$state <- paste("Parsed", private$..admin$name, "using parseFast method.")
      self$logIt()

      # Assign its name in the global environment
      assign(private$..admin$name, self, envir = .GlobalEnv)

      invisible(self)
    },

    parseFull = function(numbers = FALSE, punct = FALSE,
                         symbols = FALSE,  twitter = FALSE, hyphens = FALSE,
                         url = FALSE, email = FALSE, control = FALSE,
                         repeatChars = FALSE, longWords = FALSE) {

      private$..admin$methodName <- 'parseFull'

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
          gsub(NLPStudio:::regexPatterns$emails, ' ', x, perl = TRUE)
        })
      }

      if (control == TRUE) {
        private$..content <- lapply(private$..content, function(x) {
          gsub(NLPStudio:::regexPatterns$control, ' ', x, perl = TRUE)
        })
      }

      if (repeatChars == TRUE) {
        private$..content <- lapply(private$..content, function(x) {
          gsub(NLPStudio:::regexPatterns$repeatedChars, ' ', x, perl = TRUE)
        })
      }

      if (longWords == TRUE) {
        private$..content <- lapply(private$..content, function(x) {
          gsub(NLPStudio:::regexPatterns$longWords, ' ', x, perl = TRUE)
        })
      }

      # Logit
      private$..admin$modified <- Sys.time()
      private$..admin$state <- paste("Parsed", private$..admin$name, "using parseFull method.")
      self$logIt()

      # Assign its name in the global environment
      assign(private$..admin$name, self, envir = .GlobalEnv)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                     Text Normalization Methods                          #
    #-------------------------------------------------------------------------#
    normalize = function() {

      private$..admin$methodName <- 'normalize'

      key <- paste0("\\b", NLPStudio:::norms$key, "\\b")
      for (i in 1:length(key)) {
        private$..content <- gsub(key[i], NLPStudio::norms$value[i],
                                  private$..content,
                                  ignore.case = TRUE, perl = TRUE)
      }

      # Logit
      private$..admin$modified <- Sys.time()
      private$..admin$state <- paste0("Normalized ", private$..admin$name, ".")
      self$logIt()

      # Assign its name in the global environment
      assign(private$..admin$name, self, envir = .GlobalEnv)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                     Text Sanitization Methods                           #
    #-------------------------------------------------------------------------#
    sanitizeWord = function() {

      private$..admin$methodName <- 'sanitizeWord'

      key <- paste0("\\b", NLPStudio:::profanity, "\\b")
      for (i in 1:length(key)) {
        private$..content <- gsub(key[i], ' ', private$..content,
                                  ignore.case = TRUE, perl = TRUE)
      }

      # Logit
      private$..admin$modified <- Sys.time()
      private$..admin$state <- paste("Sanitized", private$..admin$name, "using sanitizeWord method.")
      self$logIt()

      # Assign its name in the global environment
      assign(private$..admin$name, self, envir = .GlobalEnv)

      invisible(self)
    },

    sanitizeSent = function() {

      private$..admin$methodName <- 'sanitizeSent'

      stringsRegex <- paste0("\\b",NLPStudio:::profanity, "\\b", collapse = '|')
      xidx <- unique(grep(stringsRegex, private$..content, ignore.case = TRUE))
      private$..content <- private$..content[-xidx]

      # Logit
      private$..admin$modified <- Sys.time()
      private$..admin$state <- paste("Sanitized", private$..admin$name, "using sanitizeSent method.")
      self$logIt()

      # Assign its name in the global environment
      assign(private$..admin$name, self, envir = .GlobalEnv)

      invisible(self)
    },

    sanitizeTag = function() {

      private$..admin$methodName <- 'sanitizeTag'

      key <- paste0("\\b", NLPStudio:::profanity, "\\b")
      for (i in 1:length(key)) {
        private$..content <- gsub(key[i], '<EXPLETIVE>', private$..content,
                                  ignore.case = TRUE, perl = TRUE)
      }

      # Logit
      private$..admin$modified <- Sys.time()
      private$..admin$state <- paste("Sanitized", private$..admin$name, "using sanitizeTag method.")
      self$logIt()

      # Assign its name in the global environment
      assign(private$..admin$name, self, envir = .GlobalEnv)

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Commit Changes                                #
    #-------------------------------------------------------------------------#
    commit = function() {

      private$..admin$methodName <- 'commit'

      # Remove extra white-space
      private$..content <-
        stringr::str_replace(gsub(NLPStudio:::regexPatterns$whiteSpace, " ",
                                  stringr::str_trim(private$..content)), "B", "b")
      private$..content <- private$..content[private$..content != ""]
      private$..content <- private$..content[private$..content != "'"]

      # Logit
      private$..admin$modified <- Sys.time()
      private$..admin$state <- paste("Commited preprocessing of", private$..admin$name, "at", Sys.time())
      self$logIt()

      # Assign its name in the global environment
      assign(private$..admin$name, self, envir = .GlobalEnv)

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
      visitor$archive(self)
    }
  )
)
