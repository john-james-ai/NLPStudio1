#==============================================================================#
#                           TextClean Command Classes                          #
#==============================================================================#

#------------------------------------------------------------------------------#
#                                 CmdText0                                     #
#------------------------------------------------------------------------------#
#' CmdText0
#'
#' \code{CmdText0} Abstract class  for the TextClean family of classes.
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
CmdText0 <- R6::R6Class(
  classname = "CmdText0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..x = character()
  ),

  public = list(
    initialize = function() { stop("Not implemented for this abstract/interface class.") },
    execute = function(x) { stop("Not implemented for this abstract/interface class.") }
  )
)

#------------------------------------------------------------------------------#
#                            CmdAddCommaSpace                                  #
#------------------------------------------------------------------------------#
#' CmdAddCommaSpace
#'
#' \code{CmdAddCommaSpace} Command for the AddCommaSpace class.
#'
#' Class that encapsulates the command to execute an object of the AddCommaSpace
#' class
#'
#' @usage CmdAddCommaSpace$new()
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
CmdAddCommaSpace <- R6::R6Class(
  classname = "CmdAddCommaSpace",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdAddCommaSpace"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- AddCommaSpace$new(x)$execute()
      return(x)
    }
  )
)

#------------------------------------------------------------------------------#
#                            CmdAddEndMark                                     #
#------------------------------------------------------------------------------#
#' CmdAddEndMark
#'
#' \code{CmdAddEndMark} Command for the AddEndMark class.
#'
#' Class that encapsulates the command to execute an object of the AddEndMark
#' class
#'
#' @usage CmdAddEndMark$new(replace = "|", endmarks = c("?", ".", "!"))
#'
#' @template textCleanParams
#' @param replace Symbol added for missing endmarks
#' @param endmarks List of endmark symbols to detect
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdAddEndMark <- R6::R6Class(
  classname = "CmdAddEndMark",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function(replace = "|", endmarks = c("?", ".", "!"), ...) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdAddEndMark"
      private$..replace <- replace
      private$..endmarks <- endmarks
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- AddEndMark$new(x, private$..replace, private..endmarks)$execute()
      return(x)
    }
  )
)

#------------------------------------------------------------------------------#
#                            CmdRemoveEmail                                    #
#------------------------------------------------------------------------------#
#' CmdRemoveEmail
#'
#' \code{CmdRemoveEmail} Command for the RemoveEmail class.
#'
#' Class that encapsulates the command to execute an object of the RemoveEmail
#' class
#'
#' @usage CmdRemoveEmail$new(replace)
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
CmdRemoveEmail <- R6::R6Class(
  classname = "CmdRemoveEmail",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdRemoveEmail"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveEmail$new(x)$execute()
      return(x)
    }
  )
)

#------------------------------------------------------------------------------#
#                            CmdRemoveHyphens                                  #
#------------------------------------------------------------------------------#
#' CmdRemoveHyphens
#'
#' \code{CmdRemoveHyphens} Command for the RemoveHyphens class.
#'
#' Class that encapsulates the command to execute an object of the RemoveHyphens
#' class
#'
#' @usage CmdRemoveHyphens$new()
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
CmdRemoveHyphens <- R6::R6Class(
  classname = "CmdRemoveHyphens",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdRemoveHyphens"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveHyphens$new(x)$execute()
      return(x)
    }
  )
)

#------------------------------------------------------------------------------#
#                            CmdRemoveNumbers                                  #
#------------------------------------------------------------------------------#
#' CmdRemoveNumbers
#'
#' \code{CmdRemoveNumbers} Command for the RemoveNumbers class.
#'
#' Class that encapsulates the command to execute an object of the RemoveNumbers
#' class
#'
#' @usage CmdRemoveNumbers$new()
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
CmdRemoveNumbers <- R6::R6Class(
  classname = "CmdRemoveNumbers",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdRemoveNumbers"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveNumbers$new(x)$execute()
      return(x)
    }
  )
)


#------------------------------------------------------------------------------#
#                            CmdRemovePunctuation                              #
#------------------------------------------------------------------------------#
#' CmdRemovePunctuation
#'
#' \code{CmdRemovePunctuation} Command for the RemovePunctuation class.
#'
#' Class that encapsulates the command to execute an object of the RemovePunctuation
#' class
#'
#' @usage CmdRemovePunctuation$new()
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
CmdRemovePunctuation <- R6::R6Class(
  classname = "CmdRemovePunctuation",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  private = list(
    ..endmark = logical(),
    ..apostrophe = logical()
  ),

  public = list(
    initialize = function(endmark = FALSE, apostrophe = FALSE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdRemovePunctuation"
      private$..endmark <- endmark
      private$..apostrophe <- apostrophe

      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemovePunctuation$new(x, endmark = private$..endmark,
                                 apostrophe = private$..apostrophe)$execute()
      return(x)
    }
  )
)

#------------------------------------------------------------------------------#
#                            CmdRemoveSymbols                                  #
#------------------------------------------------------------------------------#
#' CmdRemoveSymbols
#'
#' \code{CmdRemoveSymbols} Command for the RemoveSymbols class.
#'
#' Class that encapsulates the command to execute an object of the RemoveSymbols
#' class
#'
#' @usage CmdRemoveSymbols$new()
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
CmdRemoveSymbols <- R6::R6Class(
  classname = "CmdRemoveSymbols",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdRemoveSymbols"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveSymbols$new(x)$execute()
      return(x)
    }
  )
)


#------------------------------------------------------------------------------#
#                            CmdRemoveTwitter                                  #
#------------------------------------------------------------------------------#
#' CmdRemoveTwitter
#'
#' \code{CmdRemoveTwitter} Command for the RemoveTwitter class.
#'
#' Class that encapsulates the command to execute an object of the RemoveTwitter
#' class
#'
#' @usage CmdRemoveTwitter$new()
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
CmdRemoveTwitter <- R6::R6Class(
  classname = "CmdRemoveTwitter",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdRemoveTwitter"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveTwitter$new(x)$execute()
      return(x)
    }
  )
)


#------------------------------------------------------------------------------#
#                             CmdRemoveURL                                     #
#------------------------------------------------------------------------------#
#' CmdRemoveURL
#'
#' \code{CmdRemoveURL} Command for the RemoveURL class.
#'
#' Class that encapsulates the command to execute an object of the RemoveURL
#' class
#'
#' @usage CmdRemoveURL$new()
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
CmdRemoveURL <- R6::R6Class(
  classname = "CmdRemoveURL",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdRemoveURL"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveURL$new(x)$execute()
      return(x)
    }
  )
)

#------------------------------------------------------------------------------#
#                           CmdReplaceAbbreviations                             #
#------------------------------------------------------------------------------#
#' CmdReplaceAbbreviations
#'
#' \code{CmdReplaceAbbreviations} Command for the ReplaceAbbreviations class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceAbbreviations
#' class
#'
#' @usage CmdReplaceAbbreviations$new(abbreviation, replace = NULL, ignoreCase = TRUE )
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
CmdReplaceAbbreviations <- R6::R6Class(
  classname = "CmdReplaceAbbreviations",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  private = list(
    ..abbreviation = character(),
    ..replace = character(),
    ..ignoreCase = logical()
  ),

  public = list(
    initialize = function(abbreviation = NULL, replace = NULL, ignoreCase = TRUE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceAbbreviations"
      private$..abbreviation <- abbrevation
      private$..replace <- replace
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceAbbreviations$new(x, abbreviation = private$..abbreviation,
                                   replace = private$..replace,
                                   ignoreCase = private$..ignoreCase)$execute()
      return(x)
    }
  )
)

#------------------------------------------------------------------------------#
#                           CmdReplaceBacktick                                 #
#------------------------------------------------------------------------------#
#' CmdReplaceBacktick
#'
#' \code{CmdReplaceBacktick} Command for the ReplaceBacktick class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceBacktick
#' class
#'
#' @usage CmdReplaceBacktick$new(abbreviation, replace = NULL, ignoreCase = TRUE )
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
CmdReplaceBacktick <- R6::R6Class(
  classname = "CmdReplaceBacktick",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,


  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceBacktick"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceBacktick$new(x)$execute()
      return(x)
    }
  )
)

#------------------------------------------------------------------------------#
#                           CmdReplaceContractions                             #
#------------------------------------------------------------------------------#
#' CmdReplaceContractions
#'
#' \code{CmdReplaceContractions} Command for the ReplaceContractions class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceContractions
#' class
#'
#' @usage CmdReplaceContractions$new(contractions = NULL, ignoreCase = TRUE)
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
CmdReplaceContractions <- R6::R6Class(
  classname = "CmdReplaceContractions",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  private = list(
    ..contractions = character(),
    ..ignoreCase = logical()
  ),

  public = list(
    initialize = function(contractions = NULL, ignoreCase = TRUE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceContractions"
      private$..contractions <- contractions
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceContractions$new(x, contractions = private)$execute()
      return(x)
    }
  )
)
