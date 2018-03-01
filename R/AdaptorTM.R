#==============================================================================#
#                                   AdaptorTM                                  #
#==============================================================================#
#' AdaptorTM
#'
#' \code{AdaptorTM} Converts NLPStudio Corpus objects to and from tm package corpus objects.
#' 
#' Takes a NLPStudio Corpus or tm corpus object and creates an adaptation
#' of the object into its counterpart in the other format.
#' 
#' @usage AdaptorTM$new(x, format = "tm")
#' @usage AdaptorTM$new(x, format = "tm")$adapt()
#' 
#' @param x Object to be adapted
#' @param format The package / format into which x should be adapted. Valid values are "tm", in lower, or upper case.
#' 
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Adaptor family of classes
#' @export
AdaptorTM <- R6::R6Class(
  classname = "AdaptorTM",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Adaptor0,
  
  private = list(
    ..x = character(),
    ..format = character(),
    
    adaptTo = function() {
      
      # Extract metadata
      cMeta <- as.list(private$..x$meta())
      names(cMeta) <- colnames(private$..x$meta())
      dMeta <- as.data.frame(private$..x$docMeta())
      
      # Create named corpus vectors, one document per vector
      docs <- private$..x$getDocuments()
      content <- unlist(lapply(docs, function(d) {
        paste(d$text, collapse = "")
      }))
      
      # Create tm corpus object
      corpus <- tm::VCorpus(tm::VectorSource(x = content))
      
      # Add corpus metadata
      for (i in 1:length(cMeta)) {
        tm::tm_map(corpus, function(x) {
          NLP::meta(x, tag = names(cMeta)[[i]], type = "corpus") <- as.character(cMeta[[i]])
        })
      }
      
      # Add document metadata
      for (i in 1:ncol(dMeta)) {
        NLP::meta(corpus, tag = names(dMeta)[[i]], type = "local") <- as.character(dMeta[[i]])
      }
      
      # Add names to list
      names(corpus) <- unlist(lapply(corpus, function(x) {x$meta$name}))
      
      return(corpus)
      
    },
    
    adaptFrom = function() {
      
      # Obtain metadata
      cMeta <- NLP::meta(private$..x, type = "corpus")
      dMeta <- NLP::meta(private$..x, type = "local")
      
      # Create corpus object
      corpus <- Corpus$new(name = as.character(cMeta$name))
      
      # Add documents and metadata
      for (i in 1:length(private$..x)) {
        d <- Document$new(name = private$..x[[i]]$meta$name, 
                          content = as.character(private$..x[[i]]$text))  
        keys <- names(dMeta[[i]])
        values <- dMeta[[i]]
        idx <- unlist(lapply(values, function(v) {length(v) > 0}))
        keys <- keys[idx]
        values <- values[idx]
        for (j in 1:length(keys)) {
          d$meta(key = keys[j], value = values[[j]])
        }
        corpus <- corpus$addDocument(d)
      }
      
      # Add corpus metadata
      for (i in 1:length(cMeta)) {
        corpus$meta(key = names(cMeta[i]), value = cMeta[[i]])
      }
      
      return(corpus)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function(x, format) {
      
      private$..className <- "AdaptorTM"
      private$..methodName <- "initialize"
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()
      private$..logs <- LogR$new()
      
      private$..x <- x
      private$..format <- format
      
      if (private$validateParams()$code == FALSE) stop()
      
      private$..state <- paste0("Initiated ", private$..classname)
      self$logIt()
      
      invisible(self)
      
      
    },
    
    #-------------------------------------------------------------------------#
    #                              Visitor Method                             #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$adaptorTM(self)
    }
  )
)
