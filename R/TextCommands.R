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

    processDocument = function(document) {
      document$content <- gsub(private$..regex,
                               private$..replace,
                               document$content, perl = TRUE)
      return(document)
    }
  ),

  public = list(
    initialize = function(object, ...) { stop("Not implemented for this abstract/interface class.") },
    execute = function(object) {

      private$..methodName <- "execute"

      if ("Corpus" %in% class(object)) {
        documents <- object$getDocuments()
        for (i in 1:length(documents)) {
          doc <- private$processDocument(documents[[i]])
          object$addDocument(doc)
        }
      } else {
        object <- private$processDocument(object)
      }
      # Log it
      private$..state <- paste0("Executed ", class(self)[1], " on ",
                                object$getName(), ". ")
      self$logIt()

      return(object)
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
    processDocument = function(document) {
      document$content <- textclean::add_comma_space(document$content)
      return(document)
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
    processDocument = function(document) {
      document$content <- textclean::add_missing_endmark(document$content)
      return(document)
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
#' \code{RemoveHyphens} Removes email addresses from text.
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
#' \code{RemoveNumbers} Removes email addresses from text.
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

