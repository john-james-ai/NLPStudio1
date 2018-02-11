#==============================================================================#
#                               TextClean0                                     #
#==============================================================================#
#' TextClean0
#'
#' \code{TextClean0} Abstract class  for the TextClean family of classes.
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
TextClean0 <- R6::R6Class(
  classname = "TextClean0",
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
                                private$..x$getName(), ". ")
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
#' @template textCleanMethods
#' @template textCleanClasses
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
  inherit = TextClean0,

  public = list(
    initialize = function(x) {
      private$..className <- "AddCommaSpace"
      private$..methodName <- "initialize"
      private$..x <- x
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
#' \code{AddEndMark} Add missing endmarks.
#'
#' A wrapper for \code{\link[textclean]{add_missing_endmark}}, this class
#' detects missing endmarks and replaces them with the desired symbol.
#'
#' @usage AddEndMark$new(x, replacement = "|", endmarks = c("?", ".", "!"), ...)$execute()
#'
#' @template textCleanParams
#' @param replacement Symbol added for missing endmarks
#' @param endmarks List of endmark symbols to detect
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
AddEndMark <- R6::R6Class(
  classname = "AddEndMark",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(
    ..endmarks = character(),

    processText = function(content) {
      content <- textclean::add_missing_endmark(x = content,
                                                replacement = private$..replace,
                                                endmarks = private$..endmarks)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, replacement = "|", endmarks = c("?", ".", "!"), ...) {

      private$..className <- "AddEndMark"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "AddEndMark"
      private$..x <- x
      private$..replace <- replacement
      private$..endmarks <- endmarks
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
#' @usage RemoveEmail$new(x)$execute()
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemoveEmail <- R6::R6Class(
  classname = "RemoveEmail",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  public = list(
    initialize = function(x) {
      private$..className <- "RemoveEmail"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveEmail"
      private$..x <- x
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
#' @usage RemoveHyphens$new(x)$execute()
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemoveHyphens <- R6::R6Class(
  classname = "RemoveHyphens",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  public = list(
    initialize = function(x) {
      private$..className <- "RemoveHyphens"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveHyphens"
      private$..x <- x
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
#' @usage RemoveNumbers$new(x)$execute()
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemoveNumbers <- R6::R6Class(
  classname = "RemoveNumbers",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  public = list(
    initialize = function(x) {
      private$..className <- "RemoveNumbers"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveNumbers"
      private$..x <- x
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
#' @usage RemovePunctuation$new(x, endmark = FALSE, apostrophe = FALSE)$execute()
#'
#' @template textCleanParams
#' @param endmark Logical indicating whether to remove endmarks.
#' @param apostrophe Logical indicating whether to remove apostrophes.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemovePunctuation <- R6::R6Class(
  classname = "RemovePunctuation",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  public = list(
    initialize = function(x, endmark = FALSE, apostrophe = FALSE) {
      private$..className <- "RemovePunctuation"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemovePunctuation"
      private$..x <- x

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
#' \code{RemoveSymbols} Removes symbols.
#'
#' Removes symbols (all non-alphanumeric characters) from text.
#'
#' @usage RemoveSymbols$new(x)$execute()
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemoveSymbols <- R6::R6Class(
  classname = "RemoveSymbols",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  public = list(
    initialize = function(x) {
      private$..className <- "RemoveSymbols"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveSymbols"
      private$..x <- x
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
#' \code{RemoveTwitter} Removes twitter.
#'
#' Removes twitter handles from text.
#'
#' @usage RemoveTwitter$new(x)$execute()
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemoveTwitter <- R6::R6Class(
  classname = "RemoveTwitter",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  public = list(
    initialize = function(x) {
      private$..className <- "RemoveTwitter"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveTwitter"
      private$..x <- x
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
#' \code{RemoveURL} Removes URLs.
#'
#' Removes URLs  from text.
#'
#' @usage RemoveURL$new(x)$execute()
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemoveURL <- R6::R6Class(
  classname = "RemoveURL",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  public = list(
    initialize = function(x) {
      private$..className <- "RemoveURL"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveURL"
      private$..x <- x
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
#' This is a wrapper for the replace_abbreviations function in the QDAP package.
#' Source \url{https://cran.r-project.org/web/packages/qdap/qdap.pdf}
#'
#' Removes twitter handles from text.
#'
#' @usage RemoveTwitter$new(x)$execute()
#'
#' @template textCleanParams
#' @param abbreviation A two column key of abbreviations (column 1) and long form replacements (column 2) or a vector of abbreviations. Default is to use qdapDictionaries's abbreviations data set.
#' @param replace Vector of long form replacements if a data frame is not supplied to the abbreviation argument.
#' @param ignoreCase Logical. If TRUE replaces without regard to capitalization.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemoveAbbreviations <- R6::R6Class(
  classname = "RemoveAbbreviations",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

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
    initialize = function(x, abbreviation = qdapDictionaries::abbreviations,
                          replace = NULL, ignoreCase = TRUE) {
      private$..className <- "RemoveAbbreviations"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveAbbreviations"
      private$..x <- x
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
#' \code{ReplaceBacktick} Replace backtick
#'
#' Replaces backticks with single quotes.
#'
#' @usage ReplaceBackTick$new(x)$execute()
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceBacktick <- R6::R6Class(
  classname = "ReplaceBacktick",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  public = list(
    initialize = function(x) {
      private$..className <- "ReplaceBacktick"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceBacktick"
      private$..x <- x
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
#' \code{ReplacePatterns}  Replace
#'
#' A wrapper for \code{\link[textclean]{mgsub}} that takes a vector
#' of search terms and a vector or single value of replacements.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @template textCleanParams
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
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
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
  inherit = TextClean0,

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
#' \code{ReplaceContractions}  Replace contractions.
#'
#' A wrapper for \code{\link[textclean]{replace_contractions}} that
#' replaces contractions with long form.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceContractions$new(x, contractions = lexicon::key_contractions, ignoreCase = TRUE)$execute()
#'
#' @template textCleanParams
#' @param contractions A two column hash of contractions (column 1) and expanded
#' form replacements (column 2).  Default is to use \code{\link[lexicon]{key_contractions}} data set.
#' @param ignoreCase logical.  Should case be ignored?
#' @param \dots ignored.
#'
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceContractions} Returns a vector with contractions replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceContractions <- R6::R6Class(
  classname = "ReplaceContractions",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(
    ..contractions = character(),
    ..ignoreCase = logical(),

    processText = function(content) {
      content <- textclean::replace_contractions(x = content,
                                                 contraction.key = private$..contractions,
                                                 ignore.case = private$..ignoreCase)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, contractions = NULL,
                          ignoreCase = TRUE) {
      private$..className <- "ReplaceContractions"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceContractions"
      private$..x <- x
      private$..contractions <- contractions
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
#------------------------------------------------------------------------------#
#                         Replace Emoji                                        #
#------------------------------------------------------------------------------#
#' ReplaceEmoji
#'
#' \code{ReplaceEmoji}  Replace emojis with the words they represent.
#'
#' A wrapper for \code{\link[textclean]{replace_emojis}} that replaces
#' emojis with the words they represent.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceEmoji$new(x, emojis = NULL)$execute()
#'
#' @template textCleanParams
#' @param emojis A data.table of emojis (ASCII byte representations) and
#' corresponding word/identifier meanings.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceEmoji} Returns a vector with emojis replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceEmoji <- R6::R6Class(
  classname = "ReplaceEmoji",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(
    ..emojis = data.table(),

    processText = function(content) {
      content <- textclean::replace_emoji(x = content,
                                             emoji_dt = private$..emojis)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, emojis = NULL) {
      private$..className <- "ReplaceEmoji"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceEmoji"
      private$..x <- x
      private$..emojis <- emojis
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)



#------------------------------------------------------------------------------#
#                         Replace Emoticons                                    #
#------------------------------------------------------------------------------#
#' ReplaceEmoticon
#'
#' \code{ReplaceEmoticon}  Replace emoticons with the words they represent.
#'
#' A wrapper for \code{\link[textclean]{replace_emoticons}} that replaces
#' emoticons with the words they represent.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceEmoticon$new(x, emoticons = NULL)$execute()
#'
#' @template textCleanParams
#' @param emoticons A data.table of emoticons (graphical representations) and
#' corresponding word meanings.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceEmoticon} Returns a vector with emoticons replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceEmoticon <- R6::R6Class(
  classname = "ReplaceEmoticon",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(
    ..emoticons = data.table(),

    processText = function(content) {
      content <- textclean::replace_emoticon(x = content,
                                             emoticon_dt = private$..emoticons)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, emoticons = NULL) {
      private$..className <- "ReplaceEmoticon"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceEmoticon"
      private$..x <- x
      private$..emoticons <- emoticons
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                         Replace HTML                                         #
#------------------------------------------------------------------------------#
#' ReplaceHTML
#'
#' \code{ReplaceHTML}  Replace HTML Markup
#'
#' A wrapper for \code{\link[textclean]{replace_html}} replaces HTML markup.
#' The angle braces are removed and the HTML symbol markup is replaced
#' with equivalent symbols.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceHTML$new(x, symbol = TRUE)$execute()
#'
#' @template textCleanParams
#' @param symbol Logical. If codeTRUE the symbols are retained with appropriate replacements.
#' If FALSE they are removed.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceHTML} Returns a vector with HTML markup replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceHTML <- R6::R6Class(
  classname = "ReplaceHTML",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(
    ..symbol = logical(),

    processText = function(content) {
      content <- textclean::replace_html(x = content,
                                             symbol = private$..symbol)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, symbol = TRUE) {
      private$..className <- "ReplaceHTML"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceHTML"
      private$..x <- x
      private$..symbol <- symbol
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)


#------------------------------------------------------------------------------#
#                         Replace Internet Slang                               #
#------------------------------------------------------------------------------#
#' ReplaceInternetSlang
#'
#' \code{ReplaceInternetSlang}  Replace Internet Slang
#'
#' A wrapper for \code{\link[textclean]{replace_internet_slang}}
#' replaces internet slang.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceInternetSlang$new(x, slang = NULL, replace = NULL, ignoreCase = TRUE)$execute()
#'
#' @template textCleanParams
#' @param slang A vector of slang strings to replace.
#' @param replace A vector of strings with which to replace slang
#' @param ignoreCase Logical. If TRUE the case of slang will be ignored (replacement regardless of case)
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceInternetSlang} Returns a vector with internet slang replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceInternetSlang <- R6::R6Class(
  classname = "ReplaceInternetSlang",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(
    ..symbol = logical(),

    processText = function(content) {
      content <- textclean::replace_internet_slang(x = content,
                                         slang = private$..slang,
                                         replacement = private$..replace,
                                         ignore.case = private$..ignoreCase)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, slang = NULL, replace = NULL, ignoreCase = TRUE) {
      private$..className <- "ReplaceInternetSlang"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceInternetSlang"
      private$..x <- x
      private$..slang <- slang
      private$..replace <- replace
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                             Replace Kern                                     #
#------------------------------------------------------------------------------#
#' ReplaceKern
#'
#' \code{ReplaceKern}  Replace Kern
#'
#' A wrapper for \code{\link[textclean]{replace_kern}}
#' replaces kern. In typography kerning is the adjustment of spacing. Often,
#' in informal writing, adding manual spaces (a form of kerning) coupled
#' with all capital letters is used for emphasis. This tool looks for 3 or
#' more consecutive capital letters with spaces in between and removes the spaces.
#' Essentially, the capitalized, kerned version is replaced with the word equivalent.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceKern$new(x)$execute()
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceKern} Returns a vector with kern replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceKern <- R6::R6Class(
  classname = "ReplaceKern",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(

    processText = function(content) {
      content <- textclean::replace_kern(x = content)
      return(content)
    }
  ),

  public = list(
    initialize = function(x) {
      private$..className <- "ReplaceKern"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceKern"
      private$..x <- x
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                             Replace Names                                    #
#------------------------------------------------------------------------------#
#' ReplaceNames
#'
#' \code{ReplaceNames}  Replaces first/last names.
#'
#' A wrapper for \code{\link[textclean]{replace_names}}
#' Replaces first and last names.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceNames$new(x, names = NULL, replace = NULL)$execute()
#'
#' @template textCleanParams
#' @param names Vector of names to replace.
#' @param replace A string with which to replace names.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceNames} Returns a vector with names replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceNames <- R6::R6Class(
  classname = "ReplaceNames",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(

    ..names = character(),

    processText = function(content) {
      content <- textclean::replace_names(x = content,
                                          names = private$..names,
                                          replacement = private$..replace)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, names = NULL, replace = NULL) {
      private$..className <- "ReplaceNames"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceNames"
      private$..x <- x
      private$..names <- names
      private$..replace <- replace
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                             Replace NonAscii                                 #
#------------------------------------------------------------------------------#
#' ReplaceNonAscii
#'
#' \code{ReplaceNonAscii}  Replace Common Non-ASCII Characters.
#'
#' A wrapper for \code{\link[textclean]{replace_non_ascii}}
#' Replaces common non-ascii characters.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceNonAscii$new(x)$execute()
#' @usage ReplaceNonAscii$new(x, removeNonConverted = FALSE)$execute()
#'
#' @template textCleanParams
#' @param removeNonConverted Logical. If TRUE unmapped encodings are deleted from the string.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceNonAscii} Returns a vector with non-ascii characters replaced
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceNonAscii <- R6::R6Class(
  classname = "ReplaceNonAscii",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(

    ..removeNonConverted = logical(),

    processText = function(content) {
      content <- textclean::replace_non_ascii(x = content,
                                              remove.nonconverted = private$..removeNonConverted)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, removeNonConverted = TRUE) {
      private$..className <- "ReplaceNonAscii"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceNonAscii"
      private$..x <- x
      private$..names <- names
      private$..removeNonConverted <- removeNonConverted
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                             Replace Numbers                                  #
#------------------------------------------------------------------------------#
#' ReplaceNumbers
#'
#' \code{ReplaceNumbers}  Replace Numbers With Text Representation.
#'
#' A wrapper for \code{\link[textclean]{replace_numbers}} Replaces numeric represented numbers with words (e.g., 1001 becomes one thousand one).
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceNumbers$new(x)$execute()
#' @usage ReplaceNumbers$new(x, joinNumbers = TRUE, remove = FALSE)$execute()
#'
#' @template textCleanParams
#' @param joinNumbers Logical. If FALSE the elements of larger numbers are separated with spaces. If TRUE the elements will be joined without spaces.
#' @param remove Logical. If TRUE numbers are removed from the text.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceNumbers} Returns a vector with numbers replaced or removed.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceNumbers <- R6::R6Class(
  classname = "ReplaceNumbers",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(

    ..joinNumbers = logical(),
    ..remove = logical(),

    processText = function(content) {
      content <- textclean::replace_numbers(x = content,
                                            num.paste = private$..joinNumbers,
                                            remove = private$..remove)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, joinNumbers = FALSE, remove = FALSE) {
      private$..className <- "ReplaceNumbers"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceNumbers"
      private$..x <- x
      private$..joinNumbers <- joinNumbers
      private$..remove <- remove
      private$..removeNonConverted <- removeNonConverted
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                             Replace Ordinal                                  #
#------------------------------------------------------------------------------#
#' ReplaceOrdinal
#'
#' \code{ReplaceOrdinal}  Replace Mixed Ordinal Numbers With Text Representation
#'
#' A wrapper for \code{\link[textclean]{replace_ordinal}} Replaces mixed text/numeric
#' represented ordinal numbers with words (e.g., "1st" becomes "first").
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceOrdinal$new(x)$execute()
#' @usage ReplaceOrdinal$new(x, joinOrdinal = TRUE, remove = FALSE)$execute()
#'
#' @template textCleanParams
#' @param joinOrdinal Logical. If FALSE the elements of larger numbers are separated with spaces. If TRUE the elements will be joined without spaces.
#' @param remove Logical. If TRUE numbers are removed from the text.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceOrdinal} Returns a vector with ordinals replaced or removed.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceOrdinal <- R6::R6Class(
  classname = "ReplaceOrdinal",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(

    ..joinOrdinal = logical(),
    ..remove = logical(),

    processText = function(content) {
      content <- textclean::replace_numbers(x = content,
                                            num.paste = private$..joinOrdinal,
                                            remove = private$..remove)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, joinOrdinal = FALSE, remove = FALSE) {
      private$..className <- "ReplaceOrdinal"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceOrdinal"
      private$..x <- x
      private$..joinOrdinal <- joinOrdinal
      private$..remove <- remove
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                             Replace Symbol                                   #
#------------------------------------------------------------------------------#
#' ReplaceSymbol
#'
#' \code{ReplaceSymbol}  Replace Symbols With Word Equivalents
#'
#' A wrapper for \code{\link[textclean]{replace_symbol}} This function replaces symbols
#' with word equivalents (e.g., @ becomes "at".
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceSymbol$new(x, dollar = TRUE)$execute()
#' @usage ReplaceSymbol$new(x, dollar = FALSE, percent = TRUE)$execute()
#'
#' @template textCleanParams
#' @param dollar logical. If TRUE replaces dollar sign ($) with "dollar".
#' @param percent logical. If TRUE replaces percent sign (%) with "percent".
#' @param pound logical. If TRUE replaces pound sign (#) with "number".
#' @param at logical. If TRUE replaces at sign (@) with "at".
#' @param and logical. If TRUE replaces and sign (&) with "and".
#' @param  with logical. If TRUE replaces with sign (w/) with "with"
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceSymbol} Returns a vector with symbols replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceSymbol <- R6::R6Class(
  classname = "ReplaceSymbol",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(

    ..dollar = logical(),
    ..percent = logical(),
    ..pound = logical(),
    ..at = logical(),
    ..and = logical(),
    ..with = logical(),

    processText = function(content) {
      content <- textclean::replace_symbol(x = content,
                                            dollar = private$..dollar,
                                           percent = private$..percent,
                                           pound = private$..pound,
                                           at = private$..at,
                                           and = private$..and,
                                           with = private$..with)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, dollar = TRUE, percent = TRUE, pound = TRUE,
                          at = TRUE, and = TRUE, with = TRUE) {
      private$..className <- "ReplaceSymbol"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceSymbol"
      private$..x <- x
      private$..dollar <- dollar
      private$..percent <- percent
      private$..pound <- pound
      private$..at <- at
      private$..and <- and
      private$..with <- with
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)


#------------------------------------------------------------------------------#
#                             Replace Token                                    #
#------------------------------------------------------------------------------#
#' ReplaceToken
#'
#' \code{ReplaceToken}  Replace Tokens.
#'
#' A wrapper for \code{\link[textclean]{replace_tokens}} Replace tokens with a single substring.
#' This is much faster than mgsub if one wants to replace fixed tokens with a
#' single value or remove them all together. This can be useful for quickly
#' replacing tokens like names in string with a single value in order to reduce noise.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceToken$new(x, token, replace, ignoreCase = TRUE)$execute()
#'
#' @template textCleanParams
#' @param tokens A vector of token to be replaced.
#' @param replace A single character string to replace the tokens with. The default, NULL, replaces the tokens with nothing.
#' @param ignoreCase logical. If TRUE the case of the tokens will be ignored.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceToken} Returns a vector with tokens replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceToken <- R6::R6Class(
  classname = "ReplaceToken",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(

    ..tokens = character(),
    ..ignoreCase = logical(),

    processText = function(content) {
      content <- textclean::replace_symbol(x = content,
                                           tokens = private$..tokens,
                                           replacement = private$..replace,
                                           ignore.case = private$..ignoreCase)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, tokens, replace = NULL, ignoreCase = TRUE) {
      private$..className <- "ReplaceToken"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceToken"
      private$..x <- x
      private$..tokens <- tokens
      private$..replace <- replace
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                            Remove Extra White Space                          #
#------------------------------------------------------------------------------#
#' RemoveWhiteSpace
#'
#' \code{RemoveWhiteSpace} Removes extra white space from text.
#'
#' @usage RemoveWhiteSpace$new(x)$execute()
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
RemoveWhiteSpace <- R6::R6Class(
  classname = "RemoveWhiteSpace",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  public = list(
    initialize = function(x) {
      private$..className <- "RemoveWhiteSpace"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveWhiteSpace"
      private$..x <- x
      private$..regex <- "\\s+"
      private$..replace <- " "
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)

#------------------------------------------------------------------------------#
#                         Replace Word Elongation                              #
#------------------------------------------------------------------------------#
#' ReplaceWordElongation
#'
#' \code{ReplaceWordElongation}  Replace Word Elongations.
#'
#' A wrapper for \code{\link[textclean]{replace_numbers}} In informal writing
#' people may use a form of text embellishment to emphasize or alter word meanings
#' called elongation (a.k.a. "word lengthening"). For example, the use of "Whyyyyy" conveys
#' frustration. Other times the usage may be to be more sexy (e.g., "Heyyyy there"). Other times it
#' may be used for emphasis (e.g., "This is so gooood"). This function uses an augmented form of
#' Armstrong & Fogarty’s (2007) algorithm. The algorithm first attempts to replace the elongation
#' with known semantic replacements (optional; default is FALSE). After this the algorithm locates all
#' places were the same letter (case insensitive) appears 3 times consecutively. These elements are then
#' further processed. The matches are replaces via fgsub by first taking the elongation to it’s canonical
#' form (drop all > 1 consecutive letters to a single letter) and then replacing with the most common
#' word used in 2008 in Google’s ngram data set that takes the canonical form. If the canonical form
#' is not found in the Google data set then the canonical form is used as the replacement.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceWordElongation$new(x, impartMeaning = FALSE)$execute()
#'
#' @template textCleanParams
#' @param impartMeaning logical. If TRUE, known elongation semantics are used as replacements
#' (see textclean:::meaning_elongations for known elongation semantics and replacements).
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceWordElongation} Returns a vector with word elongations replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceWordElongation <- R6::R6Class(
  classname = "ReplaceWordElongation",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(

    ..importMeaning = logical(),

    processText = function(content) {
      content <- textclean::replace_symbol(x = content,
                                           impart.meaning = private$..impartMeaning)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, impartMeaning = FALSE) {
      private$..className <- "ReplaceWordElongation"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceWordElongation"
      private$..x <- x
      private$..impartMeaning <- impartMeaning
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
