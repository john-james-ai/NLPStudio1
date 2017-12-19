testFileCollection <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("external", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("external", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    unlink(downloadPath, recursive = TRUE)
    FileCollectionTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: FileCollection: Instantiation FileCollection"
    cat(paste0("\n",test, " Commencing\n"))

    # Instantiate
    path <- downloadPath
    name <- 'external'
    external <- FileCollection$new(name = name, path = downloadPath)

    # Evaluate
    fc <- external$exposeObject()
    stopifnot(fc$className == 'FileCollection')
    stopifnot(fc$methodName == 'initialize')
    stopifnot(fc$name == 'external')
    stopifnot(fc$path == downloadPath)
    stopifnot((Sys.time() - fc$created) < 1)
    stopifnot((Sys.time() - fc$modified) < 1)
    stopifnot((Sys.time() - fc$accessed) < 1)
    stopifnot(external$getPath() == downloadPath)
    stopifnot(dir.exists(downloadPath))


    FileCollectionTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(external)
  }

  test1 <- function(external) {
    test <- "test1: FileCollection: Download"
    cat(paste0("\n",test, " Commencing\n"))

    url <- 'http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
    fileName <- installr::file.name.from.url(url)
    downloadFilePath <- file.path(downloadPath, fileName)

    # Create download
    external <- external$download(url)

    # Evaluate
    fc <- external$exposeObject()
    stopifnot(fc$className == 'FileCollection')
    stopifnot(fc$methodName == 'download')
    stopifnot(fc$name == 'external')
    stopifnot(fc$path == downloadPath)
    stopifnot(external$getPath() == downloadPath)
    stopifnot(dir.exists(downloadPath))
    stopifnot(file.exists(downloadFilePath))
    stopifnot(fc$fileNames[[1]] == downloadFilePath)


    FileCollectionTest$logs(className = className, methodName = "download", msg = paste("Successfully downloaded file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(external)
  }



  test2 <- function(external) {
    test <- "test2: FileCollection: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    # Initialize variables
    rawPath <- "./test/testData/swiftKey/data/raw"
    name <- 'raw'
    zipFiles <- c("final/en_US/en_US.blogs.txt", "final/en_US/en_US.news.txt", "final/en_US/en_US.twitter.txt")

    blogsFile <- file.path(rawPath, "en_US.blogs.txt")
    newsFile <- file.path(rawPath, "en_US.news.txt")
    twitterFile <- file.path(rawPath, "en_US.twitter.txt")

    # Create new file collection
    raw <- FileCollection$new(name = name, path = rawPath)
    fc <- raw$exposeObject()
    stopifnot(fc$className == 'FileCollection')
    stopifnot(fc$methodName == 'initialize')
    stopifnot(fc$name == 'raw')
    stopifnot(fc$path == rawPath)
    stopifnot(raw$getPath() == rawPath)
    stopifnot(dir.exists(rawPath))

    # Unzip into the new collection
    raw <- raw$unZipFile(external$getFilePaths[[1]], zipFiles = zipFiles)
    files <- raw$geFilePaths()
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


downloadPath <- "./test/testData/swiftKey/data/external"

init()
external <<- test0()
external <<- test1(external)
#raw <<- test2(external)

}
className <- "File"
#source('./test/unitTests/testCorpusBuilder.R')
testFileCollection()
