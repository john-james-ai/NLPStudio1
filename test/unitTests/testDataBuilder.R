testFileCollection <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("swiftKey", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("swiftKey", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    unlink(downloadPath, recursive = TRUE)
    DataBuilderTest <<- LogTest$new()
  }



  test0 <- function() {
    test <- "test0: DataBuilder: Instantiate"
    cat(paste0("\n",test, " Commencing\n"))

    # Instantiate
    name <- 'swiftKey'
    path <- './test/testData/swiftKey'
    builder <- DataBuilder$new(name = name, path = path)

    # Evaluate
    db <- builder$exposeObject()
    stopifnot(db$className == 'DataBuilder')
    stopifnot(db$methodName == 'initialize')
    stopifnot(db$name == 'swiftKey')
    stopifnot(db$path == path)
    stopifnot((Sys.time() - db$created) < 1)
    stopifnot((Sys.time() - db$modified) < 1)
    stopifnot((Sys.time() - db$accessed) < 1)
    stopifnot(dir.exists(path))


    DataBuilderTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(builder)
  }

  test1 <- function(builder) {
    test <- "test1: FileCollection: Download External Data"
    cat(paste0("\n",test, " Commencing\n"))

    url <- 'http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
    directory <- file.path('./test/testData/swiftKey', 'external')
    filePath <- file.path(directory, "Coursera-SwiftKey.zip")


    # Download external data
    builder <- builder$download(url = url, name = 'external')

    # Evaluate
    db <- builder$exposeObject()
    fc <- fc$corpora[['external']]
    fc <- fc$exposeObject()
    stopifnot(fc$className == 'FileCollection')
    stopifnot(fc$methodName == 'download')
    stopifnot(fc$name == 'external')
    stopifnot(fc$path == directory)
    stopifnot(fc$getPath() == directory)
    stopifnot(dir.exists(directory))
    stopifnot(file.exists(filePath))
    stopifnot(fc$files[[1]]$getPath() == filePath)
    stopifnot(fc$files[[1]]$getFileName() == 'Coursera-SwiftKey.zip')


    DataBuilderTest$logs(className = className, methodName = "download", msg = paste("Successfully downloaded file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(builder)
  }



  test2 <- function(builder) {
    test <- "test2: FileCollection: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    # Initialize variables
    name <- 'raw'
    swiftKeyFiles <- c("final/en_US/en_US.blogs.txt", "final/en_US/en_US.news.txt", "final/en_US/en_US.twitter.txt")

    # Get file collection to unzip
    fc <- builder$getCorpora()[['external']]

    # Unzip
    builder <- builder$unZipFile()
    builder <- builder$unZip()
    rawData <- FileCollection$new(name = name, path = rawPath)
    rd <- rawData$exposeObject()
    stopifnot(rd$path == rawPath)

    # Unzip into the new collection




    DataBuilderTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: FileCollection: Unzip"
    cat(paste0("\n",test, " Commencing\n"))


    DataBuilderTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testData/swiftKey/data/external"

init()
builder <<- test0()
swiftKey <<- test1(builder)


}
className <- "File"
#source('./test/unitTests/testCorpusBuilder.R')
testFileCollection()
