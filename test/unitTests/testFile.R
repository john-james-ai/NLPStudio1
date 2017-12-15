testFile <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("blogs", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("blogs", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("brdata", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("brdata", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("brdsdata", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("brdsdata", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("bcsvdata", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("bcsvdata", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    FileTest <<- LogTest$new()
    file.copy('./test/testData/hc/en_US.blogs.txt', 'test/testData/en_US.blogs.txt')
    textData <- readLines(con = 'test/testData/en_US.blogs.txt')
    textData <- textData[1:2000]
    writeLines(textData, con = 'test/testData/en_US.blogs.txt')
    rm(textData)
  }

  test0 <- function() {
    test <- "test0: File: Instantiation file"
    cat(paste0("\n",test, " Commencing\n"))

    # Validation
    # blogs <- FileTXT$new()# should fail, no name
    # blogs <- FileTXT$new(name = 'blogs')# should fail -success, no path

    # Instantiate
    blogs <- FileTXT$new(name = 'blogs', path = './test/testData/en_US.blogs.txt')
    d <- blogs$exposeObject()
    stopifnot(d$className == 'FileTXT')
    stopifnot(d$name == 'blogs')
    stopifnot(d$path == './test/testData/en_US.blogs.txt')
    stopifnot(!is.null(d$logs))
    stopifnot(blogs$getClassName() == "FileTXT")
    stopifnot(blogs$getName() == "blogs")
    stopifnot(blogs$getFileName() == "en_US.blogs.txt")
    stopifnot(blogs$getPath() == "./test/testData/en_US.blogs.txt")

    # Logit
    FileTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized file"))
    FileTest$logs(className = className, methodName = "getClassName", msg = paste("Successfully obtained class name"))
    FileTest$logs(className = className, methodName = "getName", msg = paste("Successfully obtained name"))
    FileTest$logs(className = className, methodName = "getFileName", msg = paste("Successfully obtained file name"))
    FileTest$logs(className = className, methodName = "getPath", msg = paste("Successfully obtained path"))
    FileTest$logs(className = className, methodName = "getIO", msg = paste("Successfully obtained IO"))
    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test1 <- function(blogs) {
    test <- "test1: File: Read"
    cat(paste0("\n",test, " Commencing\n"))

    # Read content
    blogs <- blogs$read()
    content <- blogs$getContent()
    stopifnot(length(content) > 1000)

    # Write Content)
    blogs <- blogs$write()
    b <- blogs$exposeObject()
    stopifnot((Sys.time() -  b$modified) < 1)

    # ReadBin
    blogs <- blogs$read(IOBin$new())
    content <- blogs$getContent()
    stopifnot(length(content) > 1000)
    b <- blogs$exposeObject()
    stopifnot((Sys.time() -  b$accessed) < 1)

    # WriteBin
    blogs$write(IOBin$new())
    b <- blogs$exposeObject()
    stopifnot((Sys.time() -  b$modified) < 1)

    # Logit
    FileTest$logs(className = className, methodName = "read", msg = paste("Successfully read the file in txt format"))
    FileTest$logs(className = className, methodName = "read", msg = paste("Successfully read the file in bin format"))
    FileTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote the file in txt format"))
    FileTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote the file in bin format"))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test2 <- function(blogs) {
    test <- "test2: File: Repair"
    cat(paste0("\n",test, " Commencing\n"))

    # Read content
    blogs <- blogs$repair()
    content <- blogs$getContent()
    stopifnot(length(content) > 1000)

    # Logit
    FileTest$logs(className = className, methodName = "repair", msg = paste("Successfully repaired the file"))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test3 <- function(blogs) {
    test <- "test3: File: FileRdata"
    cat(paste0("\n",test, " Commencing\n"))

    # Read content
    blogs <- blogs$read()
    content <- blogs$getContent()
    stopifnot(length(content) > 1000)

    # Write as Rdata file
    brdata <- FileRdata$new(name = 'brdata', './test/testData/brData.Rdata')
    brdata <- brdata$addContent(content)
    brdata$write()

    # Read Rdata
    brdata$read()
    content <- brdata$getContent()
    stopifnot(length(content) > 1000)
    b <- blogs$exposeObject()
    stopifnot((Sys.time() -  b$accessed) < 1)

    # Logit
    FileTest$logs(className = className, methodName = "read", msg = paste("Successfully read Rdata format."))
    FileTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote Rdata format."))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test4 <- function(blogs) {
    test <- "test4: File: FileRDS"
    cat(paste0("\n",test, " Commencing\n"))

    # Read content
    blogs <- blogs$read()
    content <- blogs$getContent()
    stopifnot(length(content) > 1000)

    # Write as RDS file
    brdsdata <- FileRDS$new(name = 'brdsdata', './test/testData/brData.RDS')
    brdsdata <- brdsdata$addContent(content)
    brdsdata$write()

    # Read RDS
    brdsdata$read()
    content <- brdsdata$getContent()
    stopifnot(length(content) > 1000)
    b <- blogs$exposeObject()
    stopifnot((Sys.time() -  b$accessed) < 1)

    # Logit
    FileTest$logs(className = className, methodName = "read", msg = paste("Successfully read RDS format."))
    FileTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote RDS format."))

    cat(paste0(test, " Completed: Success!\n"))

    return(brdsdata)
  }


  test5 <- function(blogs) {
    test <- "test5: File: FileCSV"
    cat(paste0("\n",test, " Commencing\n"))

    # Read content
    blogs <- blogs$read()
    content <- blogs$getContent()
    stopifnot(nrow(content) > 1000)

    # Write as CSV file
    bcsvdata <- FileCSV$new(name = 'bcsvdata', './test/testData/brData.CSV')
    bcsvdata <- bcsvdata$addContent(content)
    bcsvdata$write()

    # Read CSV
    bcsvdata$read()
    content <- bcsvdata$getContent()
    stopifnot(nrow(content) > 1000)
    b <- blogs$exposeObject()
    stopifnot((Sys.time() -  b$accessed) < 1)

    # Logit
    FileTest$logs(className = className, methodName = "read", msg = paste("Successfully read CSV format."))
    FileTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote CSV format."))

    cat(paste0(test, " Completed: Success!\n"))

    return(bcsvdata)
  }


init()
blogs <<- test0()
blogs <<- test1(blogs)
blogs <<- test2(blogs)
brdata <<- test3(blogs)
brdsdata <<- test4(blogs)
bcsvdata <<- test5(blogs)
}
className <- "File"
#source('./test/unitTests/testCorpusBuilder.R')
testFile()
