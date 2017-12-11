#==============================================================================#
#                             CorpusSourceWeb                                  #
#==============================================================================#
#' CorpusSourceWeb
#'
#' \code{CorpusSourceWeb} Class responsible for obtaining corpus data from web sources.
#'
#' Provides the basic interface and core methods for the CorpusSource
#' concrete classes.
#'
#' The CorpusSourceWeb family of classes is an implementation of the strategy
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows corpus
#' sourcing behavior to be defined at run time.
#'
#' @section CorpusSourceWeb Family Participants:
#'  \itemize{
#'   \item{CorpusSource0}{Abstract class that defines core methods.}
#'   \item{CorpusSourceWeb}{Class responsible for obtaining corpus data from web sources.}
#'  }
#'
#' @section CorpusSourceWeb method:
#'  \itemize{
#'   \item{\code{new(...)}}{Method not implemented for this abstract class.}
#'   \item{\code{sourceData()}}{Method not implemented for this abstract class.}
#'  }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusSource classes
#' @export
CorpusSourceWeb <- R6::R6Class(
  classname = "CorpusSourceWeb",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Document0,

  private = list(
    ..url = character(),
    ..fileNames = character(),
    ..compressed = TRUE,
    ..format = character(),
    ..files = list(),
    ..downloadPath = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                        Source Core Methods                              #
    #-------------------------------------------------------------------------#
    initialize = function(url, fileNames = NULL, compressed = TRUE, format = 'zip') {

      private$..className <- 'CorpusSourceWeb'
      private$..methodName <- 'initialize'
      private$..state <- paste0("Corpus web sourcing initiated for ", url, ".")
      private$..name <- 'corpusSourceWeb'
      private$..url <- url
      private$..fileName <- installr::file.name.from.url(url)
      private$..path <- tempdir()
      private$..downloadPath <- tempdir()
      private$..fileNames <- fileNames
      private$..compressed <- compressed
      private$..format <- format
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..logs <- LogR$new()

      # Validation
      v <- Validator$new()
      status <- v$corpusSourceWeb(self, url)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = "Error")
        stop()
      }

      # Create log entry
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)

    },

    sourceData = function() {

      private$..methodName <- 'webSource'

      # Download data
      f <- FileManager$new()
      status <- f$download(private$..url, private$..downloadPath)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Unzip data
      f <- FileManager$new()
      if (private$..compressed == TRUE) {
        if (private$..format == 'zip') {
          oldw <- getOption("warn")
          options(warn = -1)
          status[['data']] <- f$unZipFile(zipFilePath = file.path(private$..downloadPath,
                                                                  private$..fileName),
                                          exDir = file.path(private$..path),
                                          files = private$..fileNames, list = FALSE,
                                          overwrite = FALSE)
          options(warn = oldw)

          if (status[['code']] == FALSE) {
            private$..state <- status[['msg']]
            self$logIt(level = 'Error')
            stop()
          }
        } else {
          private$..state <- paste("Only 'zip' compressed formats are supported. ",
                                   "See ?", private$..className, " for further ",
                                   "assistance. ")
          self$logIt(level = 'Error')
          stop()
        }
      } else {
        private$..state <- paste("Only compressed web sources are supported. ",
                                 "See ?", private$..className, " for further ",
                                 "assistance. ")
        self$logIt(level = 'Error')
        stop()
      }

      # Repair Data
      fileNames <- list.files(path = private..path, full.names = TRUE)
      private$..files <- lapply(fileNames, function(f) {
        self$repairFile(f)
      })
      # LogIt
      private$..state <- paste("Sourced Corpus object", private$..name, "from the web.")
      private$..modified <- Sys.time()
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Repair Files                                #
    #-------------------------------------------------------------------------#
    repairFile = function(filePath) {

      private$..methodName <- 'repair'

      # Repair File
      io <- IOBin$new()
      self$read(io)
      content <- private$..content
      content[content == as.raw(0)] = as.raw(0x20)
      content[content == as.raw(26)] = as.raw(0x20)
      private$..content <-  content
      self$write(io)

      # Remove non-ASCII quotes and hyphens
      io <- IOText$new()
      self$read(io)
      content <- private$..content
      Encoding(content) <- "latin1"
      content <- enc2utf8(content)
      content <- gsub("â€™", "'", content)
      content <- gsub("â€˜", "'", content)
      content <- gsub("â€¦", "", content)
      content <- gsub("â€", "-", content)
      content <- iconv(content, "UTF-8", "ASCII", sub = "")

      # Logit
      private$..state <- paste0("Repaired ", fileName, ".")
      self$logIt()

      return(content)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpusSourceWeb(self)
    },

    #-------------------------------------------------------------------------#
    #                           Expose Object                                 #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        className	 =  private$..className ,
        name	 = 	    private$..name ,
        path	 = 	    private$..path ,
        content =     private$..content,
        files = private$..files,
        state	 = 	    private$..state ,
        logs	 = 	    private$..logs ,
        size	 = 	    private$..size ,
        modified	 = 	private$..modified ,
        created	 = 	  private$..created ,
        accessed	 = 	private$..accessed
      )
      return(o)
    }

  )
)
