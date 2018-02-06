#' @section TextCommand Class Overview:
#' The TextCommand family of classes includes the interface and
#' concrete classes, each of which perform a single text preprocessing
#' task. This family of classes is a component of a variation on the command
#' design pattern, as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). The command pattern encapsulates
#' all information required to perform an action or trigger an event
#' for a later time. Rather than invoking a method on the receiver,
#' the Invoker accepts the receiver and the commands from the client.
#' When the "execute" is invoked, the Invoker (TextSalon) processes the
#' commands against the receiver, then returns the result to
#' the client upon completion of the commands.
#'
#' \strong{TextCommand Classes:}
#' \itemize{
#'  \item addCommaSpace: Adds space after comma.
#'  \item addEndMark: Detects missing endmarks and replaces with the desired symbol.
#'  \item removeHyphens: Replaces hyphens with a single space.
#'  \item removeNumbers: Removes numbers from a text vector.
#'  \item removePunct: Removes punctuation from a text vector.
#'  \item removeSymbols: Removes symbols from a text vector.
#'  \item removeTwitter: Removes twitter characters from text
#'  \item removeUrl: Removes URL from text vector.
#'  \item replace abbreviations: Replaces abbreviations with the long form.
#'  \item replaceBackTick: Replaces back tick with single quote. Should be performed before processing contractions.
#'  \item replaceByFunction: Matches pattern and performs function on pattern
#'  \item replaceByRegex: Wrapper for gsub function. Matches based upon regex and replaces with single value.
#'  \item replaceContraction: Replaces contractions with long form.
#'  \item replaceEmoji: Replaces emojis with word equivalents.
#'  \item replaceEmoticon: Replaces emoticons with word equivalents.
#'  \item replaceHtmlMarkup: Replaces HTML markup with equivalent symbols.
#'  \item replaceKern: Looks for 3 or more consecutive capital letters with spaces in between and removes the spaces.
#'  \item replaceMulti: A wrapper for gsub that takes a vector of search terms and a vector or single value of replacements.
#'  \item replaceNames: Replaces names with string.
#'  \item replaceNonAscii: Replaces common non-ASCII characters.
#'  \item replaceNumbers: Replaces numeric represented numbers with word representations.
#'  \item replaceOrdinal: Replaces mixed text/numeric represented ordinal numbers with words.
#'  \item replaceSlang: Replaces common internet slang.
#'  \item replaceSymbol: Replaces symbols with word equivalents.
#'  \item replaceTokens: Replace tokens with single substring.  Faster than mgsub.
#'  \item replaceWhite: Replaces one or more white spaces characters with a single space.
#'  \item replaceWordElongation: Replaces elongated words with correct representation.
#'  \item tolower: Converts the chararacter set to lower case.
#'  }
#'
