#==============================================================================#
#                                   AdaptorQ                                   #
#==============================================================================#
#' AdaptorQ
#'
#' \code{AdaptorQ} Converts NLPStudio Corpus objects to and from Quanteda corpus objects.
#' 
#' Takes a NLPStudio Corpus or Quanteda corpus object and creates an adaptation
#' of the object into its counterpart in the other format.
#' 
#' @usage AdaptorQ$new(x, format = "Q")
#' @usage AdaptorQ$new(x, format = "Q")$adapt()
#' 
#' @param x Object to be adapted
#' @param format The package / format into which x should be adapted. Valid values are "q", "Quanteda" in lower, upper, or capital case.
#' 
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Adaptor family of classes
#' @export
AdaptorQ <- R6::R6Class(
  classname = "AdaptorQ",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,
  
  private = list(
    ..x = character(),
    ..format = character(),
    
    adaptTo = function() {
      
      # Extract metadata
      cMeta <- as.list(private$..x$meta()[1,])
      names(cMeta) <- colnames(private$..x$meta())
      dMeta <- private$..x$docMeta()
      
      # Create named corpus vectors, one document per vector
      docs <- private$..x$getDocuments()
      content <- unlist(lapply(docs, function(d) {
        paste(d$content, collapse = "")
      }))
      
      # Create quanteda corpus object
      corpus <- quanteda::corpus(content, docvars = dMeta, metacorpus = cMeta)
      return(corpus)
      
    },
    
    adaptFrom = function() {
      
      # Obtain metadata
      cMeta <- quanteda::metacorpus(private$..x)
      dMeta <- quanteda::docvars(private$..x)
      
      # Create corpus object
      corpus <- Corpus$new(name = as.character(cMeta$name))
      
      # Add documents
      lapply(seq_along(private$..x$documents$texts), function(t) {
        d <- Document$new(name = as.character(private$..x$documents$name[t]), 
                          content = as.character(private$..x$documents$texts[t]))
        corpus$addDocument(d)
      })
      
      # Add corpus metadata
      lapply(seq_along(cMeta), function(m) {
        corpus$meta(key = names(cMeta[m]), value = cMeta[m])
      })
      
      # Add document metadata
      keys <- names(dMeta)
      for (i in 1:ncol(dMeta)) {
        corpus$docMeta(key = keys[i], value = dMeta[,i])
      }
      return(corpus)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function(x, format) {
      
      private$..className <- "AdaptorQ"
      private$..methodName <- "initialize"
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()
      private$..logs <- LogR$new()
      
      private$..x <- x
      private$..format <- format
      
      if (private$validateParams()$code == FALSE) stop()
      
      private$..state <- paste0("Initiated ", private$..classname)
      self$logIt()
      
      invisible(self)
      
      
    },
    #-------------------------------------------------------------------------#
    #                              Adapt Method                               #
    #-------------------------------------------------------------------------#
    adapt = function() { 
      
      if ("Corpus" %in% class(private$..x)) {
        return(private$adaptTo())
      } else {
        return(private$adaptFrom())
      }
    },
    
    #-------------------------------------------------------------------------#
    #                              Visitor Method                             #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$adaptorQ(self)
    }
  )
)
