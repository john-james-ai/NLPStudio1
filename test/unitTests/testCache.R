testCache <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    files <- list.files("./test/testData/hc", full.names = TRUE)
    hc <<- lapply(files, function(f) {
      readLines(f)
    })

    CacheTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: Cache: Settings"
    cat(paste0("\n",test, " Commencing\n"))

    # Get new cache
    cache <- Cache$new()
    inv <- cache$getInventory()
    inv2 <- cache$resetInventory()

    # Test validation logic
    #cache$maxSize <- 5 # Should fail
    #cache$maxSize <- 500000 # Should fail
    #cache$maxSize <- '1000' # Should fail
    #cache$policy <- 2

    # Test effective setting
    cache$maxSize <- 10000
    cache$policy <- "LRU"
    cs <- cache$getSettings()
    stopifnot(cs$maxSize == 1000)
    stopifnot(cs$currentSize > 0)
    stopifnot(cs$trimPolicy == "LRU")

    # Test getInventory
    inv <- cache$getInventory()
    #stopifnot(nrow(inv) > 3)
    print(inv)


    CacheTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated Cache object. "))
    CacheTest$logs(className = className, methodName = "maxSize", msg = paste("Successfully set max size. "))
    CacheTest$logs(className = className, methodName = "policy", msg = paste("Successfully set trim policy. "))
    CacheTest$logs(className = className, methodName = "getCacheState", msg = paste("Successfully retrieved cache state from file. "))
    CacheTest$logs(className = className, methodName = "putCacheState", msg = paste("Successfully wrote cache state to file. "))
    CacheTest$logs(className = className, methodName = "getSettings", msg = paste("Successfully retrieved settings. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(cs)
  }

  test1 <- function(cs) {
    test <- "test1: Cache: Write New"
    cat(paste0("\n",test, " Commencing\n"))

    # Create document object
    name <- "Blogs"
    desc <- "Creating corpus from directory sources"
    text <- readLines("./test/testData/input/en_US.blogs.txt")
    blogs <- Document$new(name = name)
    cacheDir <- ".R_Cache"
    filePath <- file.path(cacheDir, paste0(class(blogs)[1], "-",
                                 blogs$getName(), "-",
                                 blogs$getId(), ".RData"))

    # Creete and read cache
    cache <- Cache$new()
    inv <- cache$getInventory()
    nFiles <- nrow(inv)

    # Write to cache
    cache$write(blogs, text)

    # Check current size and inventory
    cs2 <- cache$getSettings()
    inv2 <- cache$getInventory()
    stopifnot(cs$currentSize < cs2$currentSize)
    stopifnot(nrow(inv2) == nrow(inv) + 1)
    stopifnot(cs2$maxSize == 1000)
    stopifnot(cs2$trimPolicy == "LRU")

    # Test persistence of cache state
    cache <- Cache$new()
    cs2 <- cache$getSettings()
    stopifnot(cs$currentSize < cs2$currentSize)
    stopifnot(cs2$maxSize == 1000)
    stopifnot(cs2$trimPolicy == "LRU")

    # Check inventory data frame
    inv <- cache$getInventory()
    stopifnot(nrow(inv) > 3)
    inventory <- subset(inv, id == blogs$getId())
    stopifnot(inventory$id == blogs$getId())
    stopifnot(inventory$name == blogs$getName())
    stopifnot(inventory$size == file.size(file.path(filePath)) / 1000000)
    stopifnot(inventory$expired == FALSE)
    stopifnot(inventory$filePath == filePath)
    stopifnot(inventory$nAccessed == 0)


    CacheTest$logs(className = "Cache", methodName = "initiate", msg = paste("Successfully intantiated cache object. "))
    CacheTest$logs(className = "Cache", methodName = "write", msg = paste("Successfully saved to cache. "))
    CacheTest$logs(className = "Cache", methodName = "getCacheState", msg = paste("Successfully saved to cache. "))
    CacheTest$logs(className = "Cache", methodName = "putCacheState", msg = paste("Successfully registered item to cache inventory. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test2 <- function(blogs) {
    test <- "test2: Cache: Update existing file"
    cat(paste0("\n",test, " Commencing\n"))

    # Get current state of cache
    cache <- Cache$new()
    cs <- cache$getSettings()
    inv <- cache$getInventory()
    size <- subset(inv, id == blogs$getId(), select = size)

    # Update cache
    cache$write(blogs, hc[[1]])

    # Get updated state of cache
    cs2 <- cache$getSettings()
    inv2 <- cache$getInventory()
    size2 <- subset(inv, id == blogs$getId(), select = size)

    # Check values updated correctly
    stopifnot(cs$currentSize < cs2$currentSize)
    stopifnot(nrow(cs) == nrow(cs2))
    stopifnot(size < size2)

    CacheTest$logs(className = "Cache", methodName = "initiate", msg = paste("Successfully intantiated cache object. "))
    CacheTest$logs(className = "Cache", methodName = "write", msg = paste("Successfully saved to cache. "))
    CacheTest$logs(className = "Cache", methodName = "save", msg = paste("Successfully saved to cache. "))
    CacheTest$logs(className = "Cache", methodName = "getCacheState", msg = paste("Successfully registered item to cache inventory. "))
    CacheTest$logs(className = "Cache", methodName = "putCacheState", msg = paste("Successfully registered item to cache inventory. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  test3 <- function(blogs) {
    test <- "testn: Cache: Read"
    cat(paste0("\n",test, " Commencing\n"))

    cache <- Cache$new()
    blogsText <- cache$read(blogs)
    stopifnot(blogsText == hc[[1]])
    inv <- cache$getInventory()
    blogsEntry <- subset(inv, id == blogs$getId())
    print(blogsEntry)

    CacheTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }

  test4 <- function() {
    test <- "test4: Cache: Fill Cache"
    cat(paste0("\n",test, " Commencing\n"))

    cache <- Cache$new()
    inv <- cache$getInventory()
    blogs <- Document$new(name = "Blogo")
    news <- Document$new(name = "News")
    twitter <- Document$new(name = "Twitter")

    i <- 0
    while(cache$maxSize - cache$getSettings()$currentSize > 0) {
      i <- i + 1
      name <- paste0("news-", i)
      news <- Document$new(name = name)
      cache$write(news, hc[3])
      cache$read(news)
      print(paste0("Max size:", cache$maxSize))
      print(paste0("Current Size:", cache$getSettings()$currentSize))
    }

    twitter <- Document$new(name = "Tweets-for-twits")
    cache <- Cache$new()
    cache <- cache$write(twitter, hc[3])

    print(cache$getInventory())


    CacheTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(news)
  }

  test5 <- function(news) {
    test <- "test5: Cache: Accessed"
    cat(paste0("\n",test, " Commencing\n"))

    cache <- Cache$new()

    for (i in 1:10) {
      newsText <- cache$read(news)
    }

    print(cache$getInventory())

    CacheTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }



  testn <- function() {
    test <- "testn: Cache: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    CacheTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

#init()
cs <- test0()
blogs <- test1(cs)
test2(blogs)
blogsText <- test3(blogs)
news <- test4()
test5(news)

}
className <- "Cache"
#source('./test/unitTests/testCache.R')
testCache()
