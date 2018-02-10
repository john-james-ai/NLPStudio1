#==============================================================================#
#                               TextCommand                                    #
#==============================================================================#
#' TextCommand
#'
#' \code{TextCommand} Abstract class  for the TextClean family of classes.
#'
#' This abstract class defines a common interface and methods for the TextClean
#' family of classes.
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean classes
#' @export
TextCommand <- R6::R6Class(
  classname = "TextCommand",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..x = character(),
    ..regex = character(),
    ..replace = character(),

    processText = function(content) {
      content <- gsub(private$..regex,
                      private$..replace,
                      content, perl = TRUE)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, ...) { stop("Not implemented for this abstract/interface class.") },

    execute = function() {

      private$..methodName <- "execute"

      if ("Corpus" %in% class(private$..x)) {
        documents <- private$..x$getDocuments()
        for (i in 1:length(documents)) {
          documents[[i]]$content <- private$processText(documents[[i]]$content)
          private$..x$addDocument(documents[[i]])
        }
      } else if ("Document" %in% class(private$..x)) {
        private$..x$content <- private$processText(private$..x$content)
      } else if ("list" %in% class(private$..x)) {
        for (i in 1:length(private$..x)) {
          private$..x[[i]] <- private$processText(private$..x[[i]])
        }
      } else {
        private$..x <- private$processText(private$..x)
      }

      # Log it
      private$..state <- paste0("Executed ", class(self)[1], " on ",
                                x$getName(), ". ")
      self$logIt()

      return(private$..x)
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
#' Class adds space after comma when commas are followed by non space characters.
#'
#' @usage AddCommaSpace$new(x)$execute()
#'
#' @template textCleanParams
#'
#' @template textCleanMethods
#'
#' @template textCleanClasses
#'
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
AddCommaSpace <- R6::R6Class(
  classname = "AddCommaSpace",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand,

  public = list(
    initialize = function() {
      private$..className <- "AddCommaSpace"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "AddCommmaSpace"
      private$..regex <- "(,)([^ ])"
      private$..replace <- "\\1 \\2"
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
#' @template textCleanClasses
#' @template textCleanMethods
#'
#' @template textCleanParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
AddEndMark <- R6::R6Class(
  classname = "AddEndMark",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand,

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
#' @template textCleanClasses
#' @template textCleanMethods
#'
#' @template textCleanParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemoveEmail <- R6::R6Class(
  classname = "RemoveEmail",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand,

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
#' @template textCleanClasses
#' @template textCleanMethods
#'
#' @template textCleanParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemoveHyphens <- R6::R6Class(
  classname = "RemoveHyphens",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand,

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
#' @template textCleanClasses
#' @template textCleanMethods
#'
#' @template textCleanParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemoveNumbers <- R6::R6Class(
  classname = "RemoveNumbers",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand,

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
#' @template textCleanClasses
#' @template textCleanMethods
#'
#' @template textCleanParams
#' @param endmark Logical indicating whether to remove endmarks.
#' @param apostrophe Logical indicating whether to remove apostrophes.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemovePunctuation <- R6::R6Class(
  classname = "RemovePunctuation",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand,

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
#' @template textCleanClasses
#' @template textCleanMethods
#'
#' @template textCleanParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemoveSymbols <- R6::R6Class(
  classname = "RemoveSymbols",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand,

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
#' @template textCleanClasses
#' @template textCleanMethods
#'
#' @template textCleanParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemoveTwitter <- R6::R6Class(
  classname = "RemoveTwitter",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand,

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
#' @template textCleanClasses
#' @template textCleanMethods
#'
#' @template textCleanParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemoveURL <- R6::R6Class(
  classname = "RemoveURL",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand,

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
#' @template textCleanClasses
#' @template textCleanMethods
#'
#' @template textCleanParams
#' @param abbreviation A two column key of abbreviations (column 1) and long form replacements (column 2) or a vector of abbreviations. Default is to use qdapDictionaries's abbreviations data set.
#' @param replace Vector of long form replacements if a data frame is not supplied to the abbreviation argument.
#' @param ignorCase Logical. If TRUE replaces without regard to capitalization.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemoveAbbreviations <- R6::R6Class(
  classname = "RemoveAbbreviations",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand,

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
#' @template textCleanClasses
#' @template textCleanMethods
#'
#' @template textCleanParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceBacktick <- R6::R6Class(
  classname = "ReplaceBacktick",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand,

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
#' @template textCleanClasses
#' @template textCleanMethods
#'
#' @template textCleanParams
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
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplacePatterns <- R6::R6Class(
  classname = "ReplacePatterns",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand,

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


#------------------------------------------------------------------------------#
#                         Replace Contractions                                 #
#------------------------------------------------------------------------------#
#' ReplaceContractions
#'
#' \code{ReplaceContractions}  - A wrapper for \code{\link[textclean]{replace_contractions}} that
#' replaces contractions with long form.
#'
#' @template textCleanClasses
#' @template textCleanMethods
#'
#' @template textCleanParams
#' @param x The text variable.
#' @param contraction.key A two column hash of contractions (column 1) and expanded
#' form replacements (column 2).  Default is to use \code{\link[lexicon]{key_contractions}} data set.
#' @param ignore.case logical.  Should case be ignored?
#' @param \dots ignored.
#' @keywords contraction
#' @examples
#' \dontrun{
#' x <- c("Mr. Jones isn't going.",
#'     "Check it out what's going on.",
#'     "He's here but didn't go.",
#'     "the robot at t.s. wasn't nice",
#'     "he'd like it if i'd go away")
#'
#' replace_contraction(x)
#' }
#'
#' @return \code{ReplaceContractions} Returns a vector with contractions replaced.
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceContractions <- R6::R6Class(
  classname = "ReplaceContractions",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCommand,

  private = list(

    processText = function(content) {
      content <- textclean::replace_contractions(x = content)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, pattern, replacement, leadspace = FALSE,
                          trailspace = FALSE, fixed = TRUE, trim = FALSE,
                          order.pattern = fixed, ...) {
      private$..className <- "ReplaceContractions"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceContractions"
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


