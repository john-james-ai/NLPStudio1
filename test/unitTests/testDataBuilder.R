testFileCollection <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("builder", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("builder", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("swiftKey", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("swiftKey", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    unlink(downloadPath, recursive = TRUE)
    DataBuilderTest <<- LogTest$new()
  }



  test0 <- function() {
    test <- "test0: DataBuilder: Instantiate"
    cat(paste0("\n",test, " Commencing\n"))

    # Key variables
    rawName <- 'raw'
    rawPath <- './test/testData/raw'
    newName <- 'foo'
    newPath <- './test/testData/foo'
    unlink(newPath, recursive = TRUE)

    # Instantiate builder and raw data
    builder <- DataBuilder$new()
    raw <- FileCollection$new(name = rawName, path = rawPath)


    # Repair raw data
    refined <- builder$repair(raw, name = newName, path = newPath)

    # Evaluate
    fc <- refined$exposeObject()
    stopifnot(length(fc$files) == 3)
    stopifnot(dir.exists(newPath))
    stopifnot(file.exists(file.path(newPath, 'en_US.blogs.txt')))
    stopifnot(file.exists(file.path(newPath, 'en_US.news.txt')))
    stopifnot(file.exists(file.path(newPath, 'en_US.twitter.txt')))

    # Read data (should get no warnings)
    content <- raw$read()

    # Evaluate Data Object
    stopifnot(length(content) == 3)

    DataBuilderTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated DataBuilder and Data objects. "))
    DataBuilderTest$logs(className = className, methodName = "repair", msg = paste("Successfully repaired file collection. "))

    cat(paste0(test, " Completed: Success!\n"))

    return(refined)
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
refined <- test0()



}
className <- "File"
#source('./test/unitTests/testCorpusBuilder.R')
testFileCollection()
