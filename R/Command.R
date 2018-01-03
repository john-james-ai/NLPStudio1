#==============================================================================#
#                                  Command                                     #
#==============================================================================#
#
#------------------------------------------------------------------------------#
#                                     C0                                       #
#------------------------------------------------------------------------------#
#' C0
#'
#' \code{C0} Abstract class for the Command classes.
#'
#' Abstract class for the Command classes.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(builder)}}{Not implemented for this abstract class.}
#'  \item{\code{execute()}}{Not implemented for this abstract class.}
#' }
#'
#' @param builder Builder object
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Command classes
#' @export
C0 <- R6::R6Class(
  "C0",
  private = list(
    ..builder = character()
  ),

  public = list(
    initialize = function(builder) stop("The method is not implemented for this abstract class."),
    execute = function() stop("The method is not implemented for this abstract class.")
  )
)
#------------------------------------------------------------------------------#
#                                  CSourceData                                 #
#------------------------------------------------------------------------------#
#' CSourceData
#'
#' \code{CSourceData} Command for sourcing FileCollection data
#'
#' Class responsible for sourcing file collection data from various sources
#' including, character vectors, lists, directories, and web sources.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(builder, dataSource)}}{Instantiates a command object }
#'  \item{\code{execute()}}{Invokes the command on the DataBuilder object (receiver)}
#' }
#'
#' @param builder Builder object
#' @param dataSource Object of one of the DataSource classes
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Command classes
#' @export
CSourceData <- R6::R6Class(
  "CSourceData",
  inherit = C0,
  private = list(
    ..dataSource = character()
  ),

  public = list(
    initialize = function(builder, dataSource) {
      private$..builder <- builder
      private$..dataSource <- dataSource
      invisible(self)
    },

    execute = function() {
      return(private$..builder$sourceData(private$..dataSource))
    }
  )
)
#------------------------------------------------------------------------------#
#                                   CRepair                                    #
#------------------------------------------------------------------------------#
#' CRepair
#'
#' \code{CRepair} Command for repairing FileCollection data
#'
#' Class responsible for repairing file collection data.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(builder, fileCollection, name, path)}}{Instantiates the repair command object}
#'  \item{\code{execute()}}{Invokes the repair command on the DataBuilder (receiver).}
#' }
#'
#' @param builder Builder object
#' @param fileCollection FileCollection object
#' @param name Character string containing the name for the repaired FileCollection object
#' @param path Character string containing the directory path containing the file collection.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Command classes
#' @export
CRepair <- R6::R6Class(
  "CRepair",
  inherit = C0,
  private = list(
    ..inFc = character(),
    ..outFc = character()
  ),

  public = list(
    initialize = function(builder, inFc, ouFc) {
      private$..builder <- builder
      private$..inFc <- inFc
      private$..outFc <- outFc
      invisible(self)
    },

    execute = function() {
      return(private$..builder$repair(private$..inFc, private$..outFc))
    }
  )
)
#------------------------------------------------------------------------------#
#                                   CReshape                                   #
#------------------------------------------------------------------------------#
#' CReshape
#'
#' \code{CReshape} Command for reshaping a FileCollection
#'
#' Class responsible for reshaping file collection data into sentences.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(builder, fileCollection, name ,path)}}{Instantiates the repair command object}
#'  \item{\code{execute()}}{Invokes the repair command on the DataBuilder (receiver).}
#' }
#'
#' @param builder Builder object
#' @param fileCollection FileCollection object
#' @param name Character string containing the name for the repaired FileCollection object
#' @param path Character string containing the directory path containing the file collection.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Command classes
#' @export
CReshape <- R6::R6Class(
  "CReshape",
  inherit = C0,
  private = list(
    ..fileCollection = character(),
    ..name = character(),
    ..path = character()
  ),

  public = list(
    initialize = function(builder, fileCollection, name, path) {
      private$..builder <- builder
      private$..fileCollection <- fileCollection
      private$..name <- name
      private$..path <- path
      invisible(self)
    },

    execute = function() {
      return(private$..builder$reshape(private$..fileCollection, private$..name, private$..path))
    }
  )
)
#------------------------------------------------------------------------------#
#                                   CSplitHoldOut                              #
#------------------------------------------------------------------------------#
#' CSplitHoldOut
#'
#' \code{CSplitHoldOut} Command for splitting a FileCollection into training, validation and test sets
#'
#' Class responsible for splitting a FileCollection object into training, validation
#' and test sets.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(builder, fileCollection, pTrain, pVal, pTest)}}{Instantiates the repair command object}
#'  \item{\code{execute()}}{Invokes the repair command on the DataBuilder (receiver).}
#' }
#'
#' @param builder Builder object
#' @param fileCollection FileCollection object
#' @param cvStrategy CVStrategy object defining the cross-validation strategy
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Command classes
#' @export
CSplitHoldOut <- R6::R6Class(
  "CSplitHoldOut",
  inherit = C0,
  private = list(
    ..fileCollection = character(),
    ..cvStrategy = character()
  ),

  public = list(
    initialize = function(builder, fileCollection, cvStrategy) {
      private$..builder <- builder
      private$..fileCollection <- fileCollection
      private$..cvStrategy <- cvStrategy
      invisible(self)
    },

    execute = function() {
      return(private$..builder$splitHoldOut(private$..fileCollection, private$..cvStrategy))
    }
  )
)
#------------------------------------------------------------------------------#
#                                   CSplitKFold                                #
#------------------------------------------------------------------------------#
#' CSplitKFold
#'
#' \code{CSplitKFold} Command for splitting a FileCollection into k folds
#'
#' Class responsible for invoking the DataBuilder method that creates k-fold training,
#' validation and test sets from a fileCollection
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(builder, fileCollection, pTrain, pVal, pTest)}}{Instantiates the repair command object}
#'  \item{\code{execute()}}{Invokes the repair command on the DataBuilder (receiver).}
#' }
#'
#' @param builder Builder object
#' @param fileCollection FileCollection object
#' @param cvStrategy CVStrategy object defining the cross-validation strategy
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Command classes
#' @export
CSplitKFold <- R6::R6Class(
  "CSplitKFold",
  inherit = C0,
  private = list(
    ..fileCollection = character(),
    ..cvStrategy = character()
  ),

  public = list(
    initialize = function(builder, fileCollection, cvStrategy) {
      private$..builder <- builder
      private$..fileCollection <- fileCollection
      private$..cvStrategy <- cvStrategy
      invisible(self)
    },

    execute = function() {
      return(private$..builder$splitKFold(private$..fileCollection, private$..cvStrategy))
    }
  )
)
