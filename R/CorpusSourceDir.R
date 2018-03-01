#==============================================================================#
#                               CorpusSourceDir                                #
#==============================================================================#
#' CorpusSourceDir
#'
#' \code{CorpusSourceDir} Creates Corpus object from directory sources.
#'
#' @template corpusSourceStrategyClasses
#' @template corpusSourceStrategyMethods
#' @template corpusSourceStrategyParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder family of classes
#' @export
CorpusSourceDir <- R6::R6Class(
  classname = "CorpusSourceDir",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CorpusSource0,
  
  private = list(
    ..repair = logical(),
    
    repairDocument = function(path) {
      
      ioBin <- IOBin$new()
      ioText <- IOText$new()
      
      content <- ioBin$read(path = path)
      
      for (i in 1:nrow(NLPStudio:::ctrl)) {
        content[content == as.raw(NLPStudio:::ctrl$pattern[i])] = as.raw(NLPStudio:::ctrl$replace[i])
      }
      
      # Save to temp file and read
      d <- tempfile(fileext = '.txt')
      ioBin$write(path = d, content = content)
      content <- ioText$read(path = d)
      
      return(content)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function(name, dataSource, repair = TRUE) {

      private$..dataSource <- dataSource
      private$..repair <- repair

      private$..className <- 'CorpusSourceDir'
      private$..methodName <- 'initialize'
      
      private$..logs <- LogR$new()

      if (private$validateParams()$code == FALSE) stop()

      private$..corpus <- Corpus$new(name = name)

      # Create log entry
      private$..state <- paste0("Corpus Directory Import object instantiated")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                              Build Methods                              #
    #-------------------------------------------------------------------------#
    build = function() {

      private$..methodName <- "build"

      # Extract file names
      if (isDirectory(private$..dataSource)) {
        files <- list.files(private$..dataSource, full.names = TRUE)
      } else {
        path <- private$..dataSource
        glob <- basename(path)
        dir <- dirname(path)
        files <- list.files(dir, pattern = glob2rx(glob), full.names = TRUE)
      }
      
      # Obtain content  
      if (private$..repair == TRUE) {
        content <- lapply(files, function(f) {
          private$repairDocument(f)
        })
      } else {
        content <- lapply(files, function(f) {
          io <- IOFactory$new(f)$getIOStrategy()
          io$read(path = f)
        })
      }
      
      # Extract meta data
      fileNames <- basename(files)
      docNames <- tools::file_path_sans_ext(fileNames)
      source <- dirname(files)

      # Create documents
      docs <- lapply(seq_along(content), function(x) {
        doc <- Document$new(name = docNames[x])
        doc$text <- content[[x]]
        doc$meta(key = "fileName", value = fileNames[x])
        doc$meta(key = "filePath", value = files[x])
        doc$meta(key = "source", value = source[x])
        doc
      })

      # Add documents to corpus
      for (i in 1:length(docs)) {
        private$..corpus <- private$..corpus$addDocument(docs[[i]])
      }

      private$..state <- paste0("Created raw corpus ", private$..name, ".")
      self$logIt()

      invisible(self)
    },

    getResult = function() {

      private$..methodName <- 'getResult'
      private$..state <- "Returned corpus object to calling environment"
      self$logIt()
      return(private$..corpus)
    },

    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpusSourceDir(self)
    }
  )
)
