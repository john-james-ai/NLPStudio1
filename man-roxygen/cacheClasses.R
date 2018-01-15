#' @section Cache Family of Classes
#' The Cache family of classes provide computation and memory management
#' services for the NLPStudio package. Momoization is implemented for process
#' instensive methods in which the results are serialized and stored on disc
#' for later use. When the method is called with the same parameters, functionality
#' checks the global environment, then the cache for the results.  If the object
#' is found, it is returned.  If not, the method executes as normal. In addition
#' memory intensive objects are stored to cache, while a memento is retained
#' in memory.
#'
#' \strong{Cache Family Participants:}
#' The Cache family of classes include the following:
#' \itemize{
#'  \item Entity: An abstract base class that defines methods common to all domain objects.
#'  \item CacheId: Class responsible for computing and returning hash based identifiers for objects stored in the cache.
#'  \item Cache: Class responsible for reading, writing, and maintaining the cache.
#'  }
