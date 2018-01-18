testCacheManager <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    files <- list.files("./test/testData/hc", full.names = TRUE)
    hc <<- lapply(files, function(f) {
      readLines(f)
    })

    CacheManagerTest <<- LogTest$new()
    directory <<- ".R_CacheManager"
  }

  test0 <- function() {

    cache <- CacheManager$new()$exposeObject()
    cache$getInstance()
    cache$maxSize <- 5 # Should fail
    cache$maxSize <- 500000 # Should fail
    cache$maxSize <- '1000' # Should fail
    cache$maxSize <- 1000 # Should be ok


    CacheManagerTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }

  test1 <- function() {
    test <- "test1: CacheManager: Write"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from directory source
    name <- "Blogs"
    desc <- "Creating corpus from directory sources"
    text <- readLines("./test/testData/input/en_US.blogs.txt")
    blogs <- Document$new(name = name)
    blogs$content <- text
    filePath <- file.path(directory,
                          paste0(class(blogs)[1],"-",
                                 name, "-",
                                 blogs$getId(), ".rdata"))

    # Get cache
    cache <- CacheManager$new()$printCache()

    # Check cache item variable (last item written to cache)
    stopifnot(nrow(cache$inventory) == 1)
    stopifnot(cache$maxSize == 2000)
    stopifnot(cache$currentSize == file.size(filePath) / 1000000)
    stopifnot(cache$item$id == blogs$getId())
    stopifnot(cache$item$name == name)
    stopifnot(cache$item$size == file.size(filePath) / 1000000)
    stopifnot(cache$item$expired == FALSE)
    stopifnot(cache$item$filePath == filePath)
    stopifnot(cache$item$nAccessed == 0)

    # Check inventory data frame
    stopifnot(nrow(cache$inventory) == 1)
    inventory <- subset(cache$inventory, id == blogs$getId())
    stopifnot(inventory$id == blogs$getId())
    stopifnot(inventory$name == name)
    stopifnot(inventory$size == file.size(filePath) / 1000000)
    stopifnot(inventory$expired == FALSE)
    stopifnot(inventory$filePath == filePath)
    stopifnot(inventory$nAccessed == 0)

    # Confirm file exists
    stopifnot(file.exists(filePath))

    name <- "MIT"
    dataSource <- "./test/testData/hc"
    corpus <- Corpus$new(name, dataSource)$build()$getResult()
    CacheManagerContent <- CacheManager$read()
    stopifnot(length(CacheManagerContent) == 3)
    CacheManagerDocuments <- CacheManager$getDocuments()
    stopifnot(length(CacheManagerDocuments) == 3)
    CacheManager$meta(key = "desc", value = desc)
    CacheManager$docMeta(key = "year", value = "2018")
    print(CacheManager$meta())
    print(CacheManager$docMeta())

    CacheManagerTest$logs(className = "CacheManager", methodName = "initiate", msg = paste("Successfully intantiated cache object. "))
    CacheManagerTest$logs(className = "CacheManager", methodName = "write", msg = paste("Successfully saved to cache. "))
    CacheManagerTest$logs(className = "CacheManager", methodName = "save", msg = paste("Successfully saved to cache. "))
    CacheManagerTest$logs(className = "CacheManager", methodName = "register", msg = paste("Successfully registered item to cache inventory. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }

  test2 <- function() {
    test <- "test2: CacheManager: Write Large Corpus Files"
    cat(paste0("\n",test, " Commencing\n"))

    name <- "MIT"
    dataSource <- "./test/testData/hc"
    corpus <- Corpus$new(name, dataSource)$build()$getResult()
    documents <- corpus$getDocuments
    ids <- lapply(documents, function(d) {
      d$getId()
    })





    CacheManagerContent <- CacheManager$read()
    stopifnot(length(CacheManagerContent) == 3)
    CacheManagerDocuments <- CacheManager$getDocuments()
    stopifnot(length(CacheManagerDocuments) == 3)
    CacheManager$meta(key = "desc", value = desc)
    CacheManager$docMeta(key = "year", value = "2018")
    print(CacheManager$meta())
    print(CacheManager$docMeta())

    CacheManagerTest$logs(className = "CacheManager", methodName = "initiate", msg = paste("Successfully intantiated cache object. "))
    CacheManagerTest$logs(className = "CacheManager", methodName = "write", msg = paste("Successfully saved to cache. "))
    CacheManagerTest$logs(className = "CacheManager", methodName = "save", msg = paste("Successfully saved to cache. "))
    CacheManagerTest$logs(className = "CacheManager", methodName = "register", msg = paste("Successfully registered item to cache inventory. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }

  testn <- function() {
    test <- "testn: CacheManager: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    CacheManagerTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
test0()
test1()


}
className <- "CacheManager"
#source('./test/unitTests/testCacheManager.R')
testCacheManager()
