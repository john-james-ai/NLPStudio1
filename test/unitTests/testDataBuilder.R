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

    # Instantiate
    name <- 'foo'
    path <- './test/testData/foo/data'
    builder <- DataBuilder$new(name = name, path = path)

    # Evaluate
    db <- builder$exposeObject()
    stopifnot(db$className == 'DataBuilder')
    stopifnot(db$methodName == 'initialize')
    stopifnot((Sys.time() - db$created) < 1)
    stopifnot((Sys.time() - db$modified) < 1)
    stopifnot((Sys.time() - db$accessed) < 1)

    # Evaluate Data Object
    stopifnot(builder$getData()$getName() == name)
    stopifnot(builder$getData()$getPath() == path)
    stopifnot(dir.exists(path))


    DataBuilderTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated DataBuilder and Data objects. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(builder)
  }

  test1 <- function(builder) {
    test <- "test1: FileCollection: Download External Data"
    cat(paste0("\n",test, " Commencing\n"))

    name <- 'external'
    url <- 'http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
    path <- file.path('./test/testData/foo/data', 'external')
    filePath <- file.path(path, "Coursera-SwiftKey.zip")


    # Download external data
    external <- builder$download(url = url, name = 'external')

    # Evaluate
    db <- external$exposeObject()
    stopifnot(db$className == 'FileCollection')
    stopifnot(db$methodName == 'download')

    # Evaluate Data Object
    stopifnot(external$getName() == name)
    stopifnot(external$getPath() == path)
    stopifnot(external$getFilePaths()[[1]] == filePath)
    stopifnot(dir.exists(path))
    stopifnot(file.exists(filePath))


    DataBuilderTest$logs(className = className, methodName = "download", msg = paste("Successfully downloaded file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(external)
  }



  test2 <- function(builder, external) {
    test <- "test2: FileCollection: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    # Initialize variables
    name <- 'raw'
    path <- "./test/testData/foo/data/raw"
    zipFiles <- c("final/en_US/en_US.blogs.txt", "final/en_US/en_US.news.txt", "final/en_US/en_US.twitter.txt")

    # Get file collection to unzip
    fc <- builder$getData()$getCollections()[['external']]

    # Unzip
    raw <- builder$unZipFile(fc = fc, name = name, zipFiles = zipFiles)

    # Evaluate
    db <- raw$exposeObject()
    stopifnot(db$className == 'FileCollection')
    stopifnot(db$methodName == 'unZipFile')

    # Evaluate Data Object
    stopifnot(raw$getName() == name)
    stopifnot(raw$getPath() == path)
    stopifnot(raw$getFilePaths()[[1]] == file.path(path, basename(zipFiles[1])))
    stopifnot(raw$getFilePaths()[[2]] == file.path(path, basename(zipFiles[2])))
    stopifnot(raw$getFilePaths()[[3]] == file.path(path, basename(zipFiles[3])))
    stopifnot(dir.exists(path))
    stopifnot(file.exists(file.path(path, basename(zipFiles[1]))))
    stopifnot(file.exists(file.path(path, basename(zipFiles[2]))))
    stopifnot(file.exists(file.path(path, basename(zipFiles[3]))))

    DataBuilderTest$logs(className = className, methodName = "unZipFile", msg = paste("Successfully unzipped file collection "))
    cat(paste0(test, " Completed: Success!\n"))

    return(raw)
  }


  test3 <- function(builder, raw) {
    test <- "test3: FileCollection: Zip"
    cat(paste0("\n",test, " Commencing\n"))

    # Initialize variables
    name <- 'zip'
    path <- "./test/testData/foo/data/zip"
    zipFilePath <- file.path(path, paste0(name, ".zip"))

    # Get file collection to unzip
    fc <- builder$getData()$getCollections()[['raw']]

    # Unzip
    zipped <- builder$zipFile(fc, name, zipFilePath)

    # Evaluate
    db <- zipped$exposeObject()
    stopifnot(db$className == 'FileCollection')
    stopifnot(db$methodName == 'zipFile')

    # Evaluate Data Object
    stopifnot(zipped$getName() == name)
    stopifnot(zipped$getPath() == path)
    stopifnot(zipped$getFilePaths() == zipFilePath)
    stopifnot(dir.exists(path))
    stopifnot(file.exists(zipFilePath))

    DataBuilderTest$logs(className = className, methodName = "unZipFile", msg = paste("Successfully unzipped file collection "))
    cat(paste0(test, " Completed: Success!\n"))

    return(zipped)
  }

  test4 <- function(builder, raw) {
    test <- "test4: FileCollection: Repair"
    cat(paste0("\n",test, " Commencing\n"))

    # Initialize variables
    name <- 'refined'
    path <- "./test/testData/foo/data/refined"

    # Get file collection to unzip
    fc <- builder$getData()$getCollections()[['raw']]

    # Repair
    refined <- builder$repair(fc, name)

    # Evaluate
    db <- refined$exposeObject()
    stopifnot(db$className == 'FileCollection')
    stopifnot(db$methodName == 'repair')

    # Evaluate Data Object
    stopifnot(refined$getName() == name)
    stopifnot(refined$getPath() == path)
    stopifnot(refined$getFilePaths()[[1]] == file.path(path, basename(files[1])))
    stopifnot(refined$getFilePaths()[[2]] == file.path(path, basename(files[2])))
    stopifnot(refined$getFilePaths()[[3]] == file.path(path, basename(files[3])))
    stopifnot(dir.exists(path))
    stopifnot(file.exists(file.path(path, basename(files[1]))))
    stopifnot(file.exists(file.path(path, basename(files[2]))))
    stopifnot(file.exists(file.path(path, basename(files[3]))))

    DataBuilderTest$logs(className = className, methodName = "repair", msg = paste("Successfully repaired file collection "))
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
builder <<- test0()
external <<- test1(builder)
raw <<- test2(builder, external)
zipped <<- test3(builder, raw)
refined <<- test4(builder, raw)


}
className <- "File"
#source('./test/unitTests/testCorpusBuilder.R')
testFileCollection()
