testFileManager <- function() {

  init <- function() {

    # Clean up
    if (file.exists(downloadPath)) unlink(downloadPath)

  }

  # Test0: Download file
  test0 <- function() {
    test <- "Test0: Download file"
    cat(paste(test, " Commencing\n"))

    fileManagerTests <<- LogTest$new()
    fm <<- FileManager$new()
<<<<<<< HEAD
    stopifnot(fm$download(lab = blue, url = url)[[1]] == TRUE)
=======
    stopifnot(fm$download(url = url, downloadPath = downloadPath)[[1]] == TRUE)
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
    stopifnot(file.exists(downloadPath))

    # Logit
    fileManagerTests$logs(className = 'FileManager', methodName = "download", msg = "Successfully downloaded file.")

    cat(paste(test, " Completed: Success!\n"))
  }

  # Test1: Unzip file
  test1 <- function() {
    test <- "Test1: Unzip file"
    cat(paste(test, " Commencing\n"))

    fileManagerTests <<- LogTest$new()
    fm <<- FileManager$new()

<<<<<<< HEAD
    stopifnot(fm$unZipFile(zipFilePath = downloadPath, exDir = exDir)[[1]] == TRUE)
=======
    stopifnot(fm$unZipFile(zipFilePath = downloadPath, files = files, exDir = exDir)[[1]] == TRUE)
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
    stopifnot(file.exists(downloadPath))

    # Logit
    fileManagerTests$logs(className = 'FileManager', methodName = "unZipFile", msg = "Successfully unzipped file.")

    cat(paste(test, " Completed: Success!\n"))
  }

  test2 <- function() {
    test <- "Test2: Zip file"
    cat(paste(test, " Commencing\n"))

    fileManagerTests <<- LogTest$new()
    fm <<- FileManager$new()
    fm$zipFile(zipFilePath = zipFilePath, files = unZipFiles)

    stopifnot(file.exists(zipFilePath))

    # Logit
    fileManagerTests$logs(className = 'FileManager', methodName = "zipFile", msg = "Successfully zipped file.")

    cat(paste(test, " Completed: Success!\n"))
  }

  test3 <- function() {
    test <- "Test3: Move File"
    cat(paste(test, " Commencing\n"))

    fileManagerTests <<- LogTest$new()
    fm <<- FileManager$new()
    from <- "./test/testData/newZip.zip"
    to <- "./test/testData/hc/newZip.zip"
    fm$moveFile(from = from, to = to)

    stopifnot(file.exists(to))

    # Logit
    fileManagerTests$logs(className = 'FileManager', methodName = "moveFile", msg = "Successfully moved file.")

    cat(paste(test, " Completed: Success!\n"))
  }

  test4 <- function() {
    test <- "Test3: Copy File"
    cat(paste(test, " Commencing\n"))

    fileManagerTests <<- LogTest$new()
    fm <<- FileManager$new()
    to <- "./test/testData/newZip.zip"
    from <- "./test/testData/hc/newZip.zip"
    fm$copyFile(from = from, to = to)

    stopifnot(file.exists(to))

    # Logit
    fileManagerTests$logs(className = 'FileManager', methodName = "copyFile", msg = "Successfully moved file.")

    cat(paste(test, " Completed: Success!\n"))
  }


<<<<<<< HEAD
  test1()
=======
  test4()
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900

}

url <- 'http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
<<<<<<< HEAD
=======
downloadPath <- './test/testData/HC-Corpus.zip'
zipFilePath <- './test/testData/newZip.zip'
exDir <- './test/testData/hc'
files <- c(file.path('final/en_US/en_US.blogs.txt'),
           file.path('final/en_US/en_US.news.txt'),
           file.path('final/en_US/en_US.twitter.txt'))
unZipFiles <- c(file.path('./test/testData/hc/en_US.blogs.txt'),
                file.path('./test/testData/hc/en_US.news.txt'),
                file.path('./test/testData/hc/en_US.twitter.txt'))
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900

testFileManager()
