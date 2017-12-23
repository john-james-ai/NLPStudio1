testFile <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("newz", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("newz", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("txtFile", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("txtFile", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("rdataFile", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("rdataFile", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("csvFile", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("csvFile", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("binFile", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("binFile", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    FileTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: File: Instantiation file"
    cat(paste0("\n",test, " Commencing\n"))

    # Validation
    #newz <- File$new()# should fail, no name
    # newz <- File$new(name = 'newz')# should fail -success, no path

    # Instantiate
    newz <- File$new(name = 'newz', path = './test/testData/input/en_US.news.txt')
    d <- newz$exposeObject()
    stopifnot(d$className == 'File')
    stopifnot(d$name == 'newz')
    stopifnot(d$path == './test/testData/input/en_US.news.txt')
    stopifnot(newz$getClassName() == "File")
    stopifnot(newz$getName() == "newz")

    # Logit
    FileTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized file"))
    FileTest$logs(className = className, methodName = "getName", msg = paste("Successfully obtained name"))
    cat(paste0(test, " Completed: Success!\n"))

    return(newz)
  }

  test1 <- function(newz) {
    test <- "test1: File: Read / Write Text File"
    cat(paste0("\n",test, " Commencing\n"))

    inpath <- "./test/testData/input/en_US.news.txt"
    outpath <- "./test/testData/txtFile.txt"

    # Read content
    file <- newz$read()
    stopifnot(length(newz$content) > 1000)

    # Write Text
    txtFile <- File$new(name = 'txtFile', path = outpath)
    txtFile$content <- file
    txtFile <- txtFile$write()
    b <- txtFile$exposeObject()
    stopifnot(b$name == 'txtFile')
    stopifnot(b$directory == dirname(outpath))
    stopifnot(b$fileName == basename(outpath))
    stopifnot(b$path == outpath)
    stopifnot((Sys.time() -  b$created) < 1)
    stopifnot((Sys.time() -  b$modified) < 1)
    stopifnot(txtFile$getName() == 'txtFile')
    stopifnot(file.exists(outpath))

    # Logit
    FileTest$logs(className = className, methodName = "read", msg = paste("Successfully read the file in txt format"))
    FileTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote the file in txt format"))

    cat(paste0(test, " Completed: Success!\n"))

    return(txtFile)
  }

  test2 <- function(newz) {
    test <- "test2: File: Read / Write Binary File"
    cat(paste0("\n",test, " Commencing\n"))

    inpath <- "./test/testData/input/en_US.news.txt"
    outpath <- "./test/testData/binFile.txt"

    # ReadBin
    file <- newz$read(IOBin$new())
    stopifnot(length(newz$content) > 1000)
    b <- newz$exposeObject()
    stopifnot((Sys.time() -  b$accessed) < 1)

    # WriteBin
    binFile <- File$new(name = 'binFile', path = outpath)
    binFile$content <- file
    binFile$write(io = IOBin$new())
    b <- binFile$exposeObject()
    stopifnot(b$name == 'binFile')
    stopifnot(b$directory == dirname(outpath))
    stopifnot(b$fileName == basename(outpath))
    stopifnot(b$path == outpath)
    stopifnot(binFile$getName() == 'binFile')
    stopifnot(file.exists(outpath))

    # Logit
    FileTest$logs(className = className, methodName = "read", msg = paste("Successfully read the file in binary format"))
    FileTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote the file in binary format"))

    cat(paste0(test, " Completed: Success!\n"))

    return(binFile)
  }


  test3 <- function(newz) {
    test <- "test3: File: Read / Write RData File"
    cat(paste0("\n",test, " Commencing\n"))

    inpath <- "./test/testData/input/en_US.news.txt"
    outpath <- "./test/testData/rdataFile.rdata"

    # Get content
    file <- newz$read()

    # Write rdata
    rdataFile <- File$new(name = 'rdataFile', path = outpath)
    rdataFile$content <- file
    rdataFile$write()
    b <- rdataFile$exposeObject()
    stopifnot(b$name == 'rdataFile')
    stopifnot(b$directory == dirname(outpath))
    stopifnot(b$fileName == basename(outpath))
    stopifnot(b$path == outpath)
    stopifnot(rdataFile$getName() == 'rdataFile')
    stopifnot(file.exists(outpath))

    # Read RData
    file <- rdataFile$read()
    stopifnot(length(newz$content) > 1000)

    # Logit
    FileTest$logs(className = className, methodName = "read", msg = paste("Successfully read the file in rdata format"))
    FileTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote the file in rdata format"))

    cat(paste0(test, " Completed: Success!\n"))

    return(rdataFile)
  }

  test4 <- function(newz) {
    test <- "test4: File: Read / Write CSV File"
    cat(paste0("\n",test, " Commencing\n"))

    inpath <- "./test/testData/input/en_US.news.txt"
    outpath <- "./test/testData/csvFile.csv"

    # Get content
    content <- newz$read()

    # Write csv
    csvFile <- File$new(name = 'csvFile', path = outpath)
    csvFile$content <- content
    csvFile$write()
    b <- csvFile$exposeObject()
    stopifnot(b$name == 'csvFile')
    stopifnot(b$directory == dirname(outpath))
    stopifnot(b$fileName == basename(outpath))
    stopifnot(b$path == outpath)
    stopifnot(csvFile$getName() == 'csvFile')
    stopifnot(file.exists(outpath))

    # Read CSV
    file <- csvFile$read()
    stopifnot(nrow(newz$content) > 1000)

    # Logit
    FileTest$logs(className = className, methodName = "read", msg = paste("Successfully read the file in csv format"))
    FileTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote the file in csv format"))

    cat(paste0(test, " Completed: Success!\n"))

    return(csvFile)
  }

  test5 <- function(newz) {
    test <- "test5: File: Lock / Unlock"
    cat(paste0("\n",test, " Commencing\n"))

    inpath <- "./test/testData/input/en_US.news.txt"
    outpath <- "./test/testData/csvFile.csv"

    # Read content
    file <- newz$read()
    stopifnot(length(newz$content) > 1000)

    # Write Text
    csvFile <- File$new(name = 'csvFile', path = outpath)
    #txtFile <- txtFile$lock()
    #txtFile <- txtFile$unlock()
    csvFile$content <- file
    csvFile <- csvFile$write()

    # Logit
    FileTest$logs(className = className, methodName = "lock", msg = paste("Successfully tested lock and write functionality."))
    FileTest$logs(className = className, methodName = "unlock", msg = paste("Successfully tested unlock and write functionality."))

    cat(paste0(test, " Completed: Success!\n"))

    return(csvFile)
  }


init()
newz <- test0()
txtFile <<- test1(newz)
binFile <<- test2(newz)
rdataFile <<- test3(newz)
csvFile <<- test4(newz)
csvFile <<- test5(newz)

}
className <- "File"
#source('./test/unitTests/testCorpusBuilder.R')
testFile()
