#==============================================================================#
#                               SourceDir                                      #
#==============================================================================#
#' SourceDir
#'
#' \code{SourceDir} Creates Corpus object from a directory source.
#'
#' @template corpusSourceStrategyClasses
#' @template corpusSourceStrategyMethods
#' @template corpusSourceStrategyParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder family of classes
#' @export
SourceDir <- R6::R6Class(
  classname = "SourceDir",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Source0,
  
  private = list(
    ..repair = logical(),
    
    repairFile = function(path) {
      
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
    initialize = function(name, corpusSource, repair = TRUE) {

      private$..source <- corpusSource
      private$..repair <- repair

      private$..className <- 'SourceDir'
      private$..methodName <- 'initialize'
      
      private$..logs <- LogR$new()

      if (private$validateParams()$code == FALSE) stop()

      private$..corpus <- Corpus$new(name = name)

      # Create log entry
      private$..state <- paste0("Corpus Directory Source object instantiated")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                              Build Methods                              #
    #-------------------------------------------------------------------------#
    build = function() {

      private$..methodName <- "build"

      # Extract file names
      if (isDirectory(private$..source)) {
        files <- list.files(private$..source, full.names = TRUE)
      } else {
        path <- private$..source
        glob <- basename(path)
        dir <- dirname(path)
        files <- list.files(dir, pattern = glob2rx(glob), full.names = TRUE)
      }
      
      # Extract meta data
      fileNames <- basename(files)
      docNames <- tools::file_path_sans_ext(fileNames)
      source <- dirname(files)
      
      # Repair file (by default) and obtain content  
      if (private$..repair == TRUE) {
        content <- lapply(files, function(f) {
          private$repairFile(f)
        })
      } else {
        content <- lapply(files, function(f) {
          io <- IOFactory$new(f)$getIOStrategy()
          io$read(path = f)
        })
      }

      # Build the hierarchy of objects. Text, Document, Corpus
      docs <- lapply(seq_along(content), function(x) {
        
        # Build the Text Object and add metadata
        text <- Text$new(content = content[[x]], type = "raw")
        text$meta(key = "fileName", value = fileNames[x])
        text$meta(key = "filePath", value = files[x])
        text$meta(key = "source", value = source[x])
        
        # Create a Document object and attach the Text Object
        doc <- Document$new(name = docNames[[x]])
        doc <- doc$attach(text)
        doc
      })

      # Attach documents to corpus
      for (i in 1:length(docs)) {
        private$..corpus <- private$..corpus$addDocument(docs[[i]])
      }

      private$..state <- paste0("Created corpus ", private$..meta[["name"]], ".")
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
      visitor$sourceDir(self)
    }
  )
)
