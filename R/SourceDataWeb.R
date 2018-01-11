#==============================================================================#
#                             SourceDataWeb                                    #
#==============================================================================#
#' SourceDataWeb
#'
#' \code{SourceDataWeb} Class responsible for obtaining corpus data from web sources.
#'
#' The corpus data are downloaded into the data/external/name subdirectory for
#' the designated pipeline and name.
#'
#' The SourceDataWeb family of classes is an implementation of the strategy
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows corpus
#' sourcing behavior to be defined at run time.
#'
#' @section SourceDataWeb Family Participants:
#'  \itemize{
#'   \item{SourceData0}{Abstract class that defines core methods.}
#'   \item{SourceDataWeb}{Class responsible for obtaining corpus data from web sources.}
#'  }
#'
#' @section SourceDataWeb methods:
#'  \itemize{
#'   \item{\code{new(...)}}{Method not implemented for this abstract class.}
#'   \item{\code{sourceData()}}{Method not implemented for this abstract class.}
#'   \item{\code{getURL()}}{Method used to return the URL, primarily for validation purposes. }
#'   \item{\code{accept()}}{Method used to accept visitor objects. }
#'  }
#'
#' @param name Character string indicating the name of the SourceDataWeb object.
#' @param desc Character string containing the description of the corpus.
#' @param pipeline Pipeline object into to which the data is to be used.
#' @param url Character string indicating the url from which the data will be sourced.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusSource classes
#' @export
SourceDataWeb <- R6::R6Class(
  classname = "SourceDataWeb",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = SourceData0,

  private = list(
    ..url = character(),
    ..compressed = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                        Source Core Methods                              #
    #-------------------------------------------------------------------------#
    initialize = function(name, url, compressed = TRUE) {

      private$..className <- 'SourceDataWeb'
      private$..methodName <- 'initialize'
      private$..state <- paste0("Web data sourcing initiated for ", url, ".")
      private$..meta[["name"]] <- name
      private$..path <- file.path(NLPStudio$new()$getInstance()$getPath(), 'externalData', name)
      private$..url <- url
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()
      private$..logs <- LogR$new()

      # Validation
      status <- list()
      status[['code']] <- TRUE
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = "Error")
        stop()
      }

      private$..fileName <- installr::file.name.from.url(url)


      # Create log entry
      self$logIt()

      # Assign its name in the global environment
      assign(private$..meta[["name"]], self, envir = .GlobalEnv)

      invisible(self)

    },

    sourceData = function() {

      private$..methodName <- 'sourceData'

      status <- list()
      status[['code']] <- TRUE

      # Create download directory
      dir.create(private$..path, showWarnings = FALSE, recursive = TRUE)

      # Download data
      if (!file.exists(private$..path)) {
        f <- FileManager$new()
        status <- f$download(private$..url, directory)
        if (status[['code']] == FALSE) {
          private$..state <- status[['msg']]
          self$logIt(level = 'Error')
          stop()
        }
      }

      # Obtain file size
      private$..fileSize <- file.size(private$..path)


      # LogIt
      private$..state <- paste("Sourced Corpus object", private$..meta[["name"]], "from the web.")
      private$..modified <- Sys.time()
      self$logIt()

      # Assign its name in the global environment
      assign(private$..meta[["name"]], self, envir = .GlobalEnv)

      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$sourceDataWeb(self)
    },

    getURL = function() private$..url,

    #-------------------------------------------------------------------------#
    #                           Expose Object                                 #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        className	 =  private$..className ,
        methodName = private$..methodName,
        name	 = 	    private$..meta[["name"]] ,
        parent = private$..parent,
        url = private$..url,
        fileName = private$..fileName,
        path	 = 	    private$..path ,
        state	 = 	    private$..state ,
        size = private$..fileSize,
        logs	 = 	    private$..logs ,
        modified	 = 	private$..modified ,
        created	 = 	  private$..created ,
        accessed	 = 	private$..accessed
      )
      return(o)
    }

  )
)
