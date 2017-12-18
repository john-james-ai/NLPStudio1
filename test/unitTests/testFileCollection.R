testFileCollection <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("news", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("news", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    FileCollectionTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: FileCollection: Instantiation FileCollection"
    cat(paste0("\n",test, " Commencing\n"))

    # Instantiate
    path <- './test/testData/collections/swift'
    name <- 'swiftKey'
    swiftKey <- FileCollection$new(name = name, path = path)

    # Evaluate
    fc <- swiftKey$exposeObject()
    stopifnot(fc$className == 'FileCollection')
    stopifnot(fc$methodName == 'initialize')
    stopifnot(fc$name == 'swiftKey')
    stopifnot(fc$path == path)
    stopifnot((Sys.time() - fc$created) < 1)
    stopifnot((Sys.time() - fc$modified) < 1)
    stopifnot((Sys.time() - fc$accessed) < 1)
    stopifnot(swiftKey$getPath() == path)
    stopifnot(dir.exists(path))


    FileCollectionTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(swiftKey)
  }

  test1 <- function(swiftKey) {
    test <- "test1: FileCollection: Download"
    cat(paste0("\n",test, " Commencing\n"))

    url <- 'http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
    directory <- 'external'
    downloadFile <- swiftKey$download(url, directory)

    stopifnot(file.exists(downloadFile))

    FileCollectionTest$logs(className = className, methodName = "download", msg = paste("Successfully downloade file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(downloadFile)
  }



  test2 <- function() {
    test <- "test2: FileCollection: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    # Unzip file



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
swiftKey <- test0()
downloadFile <- test1(swiftKey)
news <- test2(swiftKey, downloadFile)
}
className <- "File"
#source('./test/unitTests/testCorpusBuilder.R')
testFileCollection()
