testFileCollection <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("textfc", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("textfc", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("binfc", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("binfc", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("rdatafc", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("rdatafc", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("csvfc", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("csvfc", ls(envir = .GlobalEnv))], envir = .GlobalEnv)

    FileCollectionTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: FileCollection: Instantiation FileCollection"
    cat(paste0("\n",test, " Commencing\n"))

    # Instantiate
    path <- "./test/testData/input"
    name <- 'foo'
    foo <- FileCollection$new(name = name, path = path)

    # Evaluate
    fc <- foo$exposeObject()
    stopifnot(fc$className == 'FileCollection')
    stopifnot(fc$methodName == 'initialize')
    stopifnot(fc$name == 'foo')
    stopifnot(fc$path == path)
    stopifnot(dir.exists(path))


    FileCollectionTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\    binData <- foo$read(IOBin$new())
n"))

    return(foo)
  }

  test1 <- function(foo) {
    test <- "test1: FileCollection: ReadBin"
    cat(paste0("\n",test, " Commencing\n"))

    # Read binary file
    io <- IOBin$new()
    binData <- foo$read(io)
    stopifnot()

    name = 'binfc'
    path = './test/testData/binfc'

    binfc <- FileCollection$new(name, path)
    fc <- binfc$exposeObject()
    stopifnot(fc$className == 'FileCollection')
    stopifnot(fc$methodName == 'initialize')
    stopifnot(fc$name == name)
    stopifnot(fc$path == path)
    stopifnot(binfc$getPath() == path)
    stopifnot(dir.exists(path))

    # Read binary file
    io <- IOBin$new()
    binData <- binfc$read(io)
    stopifnot()


    FileCollectionTest$logs(className = className, methodName = "download", msg = paste("Successfully downloaded file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(binfc)
  }



  test2 <- function(external) {
    test <- "test2: FileCollection: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    # Initialize variables
    fooPath <- "./test/testData/swiftKey/data/foo"
    name <- 'foo'
    zipFiles <- c("final/en_US/en_US.blogs.txt", "final/en_US/en_US.news.txt", "final/en_US/en_US.twitter.txt")

    blogsFile <- file.path(fooPath, "en_US.blogs.txt")
    newsFile <- file.path(fooPath, "en_US.news.txt")
    twitterFile <- file.path(fooPath, "en_US.twitter.txt")

    # Create new file collection
    foo <- FileCollection$new(name = name, path = fooPath)
    fc <- foo$exposeObject()
    stopifnot(fc$className == 'FileCollection')
    stopifnot(fc$methodName == 'initialize')
    stopifnot(fc$name == 'foo')
    stopifnot(fc$path == fooPath)
    stopifnot(foo$getPath() == fooPath)
    stopifnot(dir.exists(fooPath))

    # Unzip into the new collection
    foo <- foo$unZipFile(zipFiles = zipFiles)
    files <- foo$geFilePaths()
    stopifnot(files[[1]] == blogsFile)
    stopifnot(files[[2]] == newsFile)
    stopifnot(files[[3]] == twitterFile)
    stopifnot(file.exists(files[[1]]))
    stopifnot(file.exists(files[[2]]))
    stopifnot(file.exists(files[[3]]))

    FileCollectionTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: FileCollection: Unzip"
    cat(paste0("\n",test, " Commencing\n"))


    FileCollectionTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }



init()
foo <- test0()
external <<- test1(external)
foo <- test2(external)

}
className <- "File"
#source('./test/unitTests/testCorpusBuilder.R')
testFileCollection()
