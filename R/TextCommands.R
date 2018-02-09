#==============================================================================#
#                               TextCommands                                   #
#==============================================================================#
#' TextCommand0
#'
#' \code{TextCommand0} Command interface class.
#'
#' @template textCommandClasses
#' @template textCommandMethods
#'
#' @template textCommandParams
#'
#' @docType class
#' @author John James, \email{jjames@@datascienceCommands.org}
#' @family TextCommands classes
#' @export
TextCommand0 <- R6::R6Class(
  classname = "TextCommand0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..object = character(),
    ..regex = character(),
    ..replace = character(),

    processText = function(content) {
      content <- gsub(private$..regex,
                      private$..replace,
                      document$content, perl = TRUE)
      return(content)
    }
  ),

  public = list(
    initialize = function(object, ...) { stop("Not implemented for this abstract/interface class.") },

    execute = function() {

      private$..methodName <- "execute"

      if ("Corpus" %in% class(private$..object)) {
        documents <- private$..object$getDocuments()
        for (i in 1:length(documents)) {
          documents[[i]]$content <- private$processText(documents[[i]]$content)
          private$..object$addDocument(documents[[i]])
        }
      } else if ("Document" %in% class(private$..object)) {
        private$..object$content <- private$processText(private$..object$content)
      } else if ("list" %in% class(private$..object)) {
        for (i in 1:length(private$..object)) {
          private$..object[[i]] <- private$processText(private$..object[[i]])
        }
      } else {
        private$..object <- private$processText(private$..object)
      }

      # Log it
      private$..state <- paste0("Executed ", class(self)[1], " on ",
                                object$getName(), ". ")
      self$logIt()

      return(private$..object)
    }
  )
)
#------------------------------------------------------------------------------#
#                              Add Comma Space                                 #
#------------------------------------------------------------------------------#
#' AddCommaSpace
#'
#' \code{AddCommaSpace} Adds space after comma.
#'
#' @template textCommandClasses
#' @template textCommandMethods
#'
#' @template textCommandParams
#'
#' @docType class
#' @author John James, \email{jjames@@datascienceCommands.org}
#' @family TextCommands classes
#' @export
AddCommaSpace <- R6::R6Class(
  classname = "AddCommaSpace",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand0,

  private = list(
    processText = function(content) {
      content <- textclean::add_comma_space(content)
      return(content)
    }
  ),

  public = list(
    initialize = function() {
      private$..className <- "AddCommaSpace"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "AddCommmaSpace"
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                              Add Missing Endmark                             #
#------------------------------------------------------------------------------#
#' AddEndMark
#'
#' \code{AddEndMark} Adds space after comma.
#'
#' @template textCommandClasses
#' @template textCommandMethods
#'
#' @template textCommandParams
#'
#' @docType class
#' @author John James, \email{jjames@@datascienceCommands.org}
#' @family TextCommands classes
#' @export
AddEndMark <- R6::R6Class(
  classname = "AddEndMark",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand0,

  private = list(
    processText = function(content) {
      content <- textclean::add_missing_endmark(content)
      return(content)
    }
  ),

  public = list(
    initialize = function() {
      private$..className <- "AddEndMark"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "AddEndMark"
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                              Remove Email Addresses                          #
#------------------------------------------------------------------------------#
#' RemoveEmail
#'
#' \code{RemoveEmail} Removes email addresses from text.
#'
#' @template textCommandClasses
#' @template textCommandMethods
#'
#' @template textCommandParams
#'
#' @docType class
#' @author John James, \email{jjames@@datascienceCommands.org}
#' @family TextCommands classes
#' @export
RemoveEmail <- R6::R6Class(
  classname = "RemoveEmail",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand0,

  public = list(
    initialize = function() {
      private$..className <- "RemoveEmail"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveEmail"
      private$..regex <- "[a-zA-Z0-9\\-_~]+(\\.[a-zA-Z0-9\\-_~]+)*@[a-zA-Z0-9\\-_~]+(\\.[a-zA-Z0-9\\-_~]+)*\\.[a-zA-Z]{2,}"
      private$..replace <- ""
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                              Remove Hyphens                                  #
#------------------------------------------------------------------------------#
#' RemoveHyphens
#'
#' \code{RemoveHyphens} Removes hyphens from text.
#'
#' @template textCommandClasses
#' @template textCommandMethods
#'
#' @template textCommandParams
#'
#' @docType class
#' @author John James, \email{jjames@@datascienceCommands.org}
#' @family TextCommands classes
#' @export
RemoveHyphens <- R6::R6Class(
  classname = "RemoveHyphens",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand0,

  public = list(
    initialize = function() {
      private$..className <- "RemoveHyphens"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveHyphens"
      private$..regex <- '[-]'
      private$..replace <- " "
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                              Remove Numbers                                  #
#------------------------------------------------------------------------------#
#' RemoveNumbers
#'
#' \code{RemoveNumbers} Removes numbers from text.
#'
#' @template textCommandClasses
#' @template textCommandMethods
#'
#' @template textCommandParams
#'
#' @docType class
#' @author John James, \email{jjames@@datascienceCommands.org}
#' @family TextCommands classes
#' @export
RemoveNumbers <- R6::R6Class(
  classname = "RemoveNumbers",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand0,

  public = list(
    initialize = function() {
      private$..className <- "RemoveNumbers"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveNumbers"
      private$..regex <- '[[:digit:]]'
      private$..replace <- ""
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                              Remove Punctuation                              #
#------------------------------------------------------------------------------#
#' RemovePunctuation
#'
#' \code{RemovePunctuation} Removes punctuation from text.
#'
#' @template textCommandClasses
#' @template textCommandMethods
#'
#' @template textCommandParams
#' @param endmark Logical indicating whether to remove endmarks.
#' @param apostrophe Logical indicating whether to remove apostrophes.
#'
#' @docType class
#' @author John James, \email{jjames@@datascienceCommands.org}
#' @family TextCommands classes
#' @export
RemovePunctuation <- R6::R6Class(
  classname = "RemovePunctuation",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand0,

  public = list(
    initialize = function(endmark = FALSE, apostrophe = FALSE) {
      private$..className <- "RemovePunctuation"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemovePunctuation"

      if (endmark == FALSE & apostrophe == FALSE) {
        private$..regex <- "(?![.?!'])[[:punct:]]"
      } else if (endmark == FALSE) {
        private$..regex <- "(?![.?!])[[:punct:]]"
      } else if (apostrophe == FALSE) {
        private$..regex <- "(?!['])[[:punct:]]"
      } else {
        private$..regex <- "[[:punct:]]"
      }
      private$..replace <- ""
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                              Remove Symbols                                  #
#------------------------------------------------------------------------------#
#' RemoveSymbols
#'
#' \code{RemoveSymbols} Removes symbols (all non-alphanumeric characters) from text.
#'
#' @template textCommandClasses
#' @template textCommandMethods
#'
#' @template textCommandParams
#'
#' @docType class
#' @author John James, \email{jjames@@datascienceCommands.org}
#' @family TextCommands classes
#' @export
RemoveSymbols <- R6::R6Class(
  classname = "RemoveSymbols",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand0,

  public = list(
    initialize = function() {
      private$..className <- "RemoveSymbols"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveSymbols"
      private$..regex <- "[^[:alnum:]]"
      private$..replace <- " "
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                              Remove Twitter                                  #
#------------------------------------------------------------------------------#
#' RemoveTwitter
#'
#' \code{RemoveTwitter} Removes twitter hnadles from text.
#'
#' @template textCommandClasses
#' @template textCommandMethods
#'
#' @template textCommandParams
#'
#' @docType class
#' @author John James, \email{jjames@@datascienceCommands.org}
#' @family TextCommands classes
#' @export
RemoveTwitter <- R6::R6Class(
  classname = "RemoveTwitter",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand0,

  public = list(
    initialize = function() {
      private$..className <- "RemoveTwitter"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveTwitter"
      private$..regex <- '\\B#\\w*[a-zA-Z]+\\w*'
      private$..replace <- " "
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                              Remove URL                                      #
#------------------------------------------------------------------------------#
#' RemoveURL
#'
#' \code{RemoveURL} Removes URLs from text.
#'
#' @template textCommandClasses
#' @template textCommandMethods
#'
#' @template textCommandParams
#'
#' @docType class
#' @author John James, \email{jjames@@datascienceCommands.org}
#' @family TextCommands classes
#' @export
RemoveURL <- R6::R6Class(
  classname = "RemoveURL",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand0,

  public = list(
    initialize = function() {
      private$..className <- "RemoveURL"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveURL"
      private$..regex <- "(?:(?:https?:\\/\\/)|(?:www\\.))[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,4}\\b(?:[-a-zA-Z0-9@:%_\\+.~#?&/=]*)"

      private$..replace <- " "
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                         Remove Abbreviations                                 #
#------------------------------------------------------------------------------#
#' RemoveAbbreviations
#'
#' \code{RemoveAbbreviations} Replaces abbreviations with long form.
#'
#' This is a wrapper for the replace_abbreviations function in the QDAP package. https://cran.r-project.org/web/packages/qdap/qdap.pdf
#'
#' @template textCommandClasses
#' @template textCommandMethods
#'
#' @template textCommandParams
#' @param abbreviation A two column key of abbreviations (column 1) and long form replacements (column 2) or a vector of abbreviations. Default is to use qdapDictionaries's abbreviations data set.
#' @param replace Vector of long form replacements if a data frame is not supplied to the abbreviation argument.
#' @param ignorCase Logical. If TRUE replaces without regard to capitalization.
#'
#' @docType class
#' @author John James, \email{jjames@@datascienceCommands.org}
#' @family TextCommands classes
#' @export
RemoveAbbreviations <- R6::R6Class(
  classname = "RemoveAbbreviations",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand0,

  private = list(
    ..abbreviation = character(),
    ..replace = character(),
    ..ignoreCase = character(),

    processText = function(content) {
      content <- qdap::replace_abbreviation(content,
                                            private$..abbreviation,
                                            private$..replace,
                                            private$..ignoreCase)
      return(content)
    }
  ),

  public = list(
    initialize = function(abbreviation = qdapDictionaries::abbreviations,
                          replace = NULL, ignoreCase = TRUE) {
      private$..className <- "RemoveAbbreviations"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveAbbreviations"
      private$..abbreviation <- abbreviation
      private$..replace <- replace
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                              Replace Backtick                                #
#------------------------------------------------------------------------------#
#' ReplaceBacktick
#'
#' \code{ReplaceBacktick} Removes URLs from text.
#'
#' @template textCommandClasses
#' @template textCommandMethods
#'
#' @template textCommandParams
#'
#' @docType class
#' @author John James, \email{jjames@@datascienceCommands.org}
#' @family TextCommands classes
#' @export
ReplaceBacktick <- R6::R6Class(
  classname = "ReplaceBacktick",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand0,

  public = list(
    initialize = function() {
      private$..className <- "ReplaceBacktick"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceBacktick"
      private$..regex <- "\`"

      private$..replace <- "'"
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                         Replace Patterns                                     #
#------------------------------------------------------------------------------#
#' ReplacePatterns
#'
#' \code{ReplacePatterns}  - A wrapper for \code{\link[textclean]{mgsub}} that takes a vector
#' of search terms and a vector or single value of replacements.
#'
#' @template textCommandClasses
#' @template textCommandMethods
#'
#' @template textCommandParams
#' @param x A character vector.
#' @param pattern Character string to be matched in the given character vector.
#' @param replacement Character string equal in length to pattern or of length
#' one which are  a replacement for matched pattern.
#' @param leadspace logical.  If \code{TRUE} inserts a leading space in the
#' replacements.
#' @param trailspace logical.  If \code{TRUE} inserts a trailing space in the
#' replacements.
#' @param fixed logical. If \code{TRUE}, pattern is a string to be matched as is.
#' Overrides all conflicting arguments.
#' @param trim logical.  If \code{TRUE} leading and trailing white spaces are
#' removed and multiple white spaces are reduced to a single white space.
#' @param order.pattern logical.  If \code{TRUE} and \code{fixed = TRUE}, the
#' \code{pattern} string is sorted by number of characters to prevent substrings
#' replacing meta strings (e.g., \code{pattern = c("the", "then")} resorts to
#' search for "then" first).
#' @param \dots Additional arguments passed to \code{\link[base]{gsub}}.
#'
#' @return \code{ReplacePatterns} - Returns a vector with the pattern replaced.
#' @docType class
#' @author John James, \email{jjames@@datascienceCommands.org}
#' @family TextCommands classes
#' @export
ReplacePatterns <- R6::R6Class(
  classname = "ReplacePatterns",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand0,

  private = list(
    ..x = character(),
    ..pattern = character(),
    ..replacement = character(),
    ..leadspace = logical(),
    ..trailspace = logical(),
    ..fixed = logical(),
    ..trim = logical(),
    ..orderPattern = character(),

    processText = function(content) {
      content <- textclean::mgsub(x = content,
                                  pattern = private$..pattern,
                                  replacement = private$..replacement,
                                  leadspace = private$..leadspace,
                                  trailspace = private$..trailspace,
                                  fixed = private$..fixed,
                                  trim = private$..trim,
                                  order.pattern = private$..orderPattern)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, pattern, replacement, leadspace = FALSE,
                          trailspace = FALSE, fixed = TRUE, trim = FALSE,
                          order.pattern = fixed, ...) {
      private$..className <- "ReplacePatterns"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplacePatterns"
      private$..x <- x
      private$..pattern <- pattern
      private$..replacement <- replacement
      private$..leadspace <- leadspace
      private$..trailspace <- trailspace
      private$..fixed <- fixed
      private$..trim <- trim
      private$..orderPattern <- order.pattern
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
