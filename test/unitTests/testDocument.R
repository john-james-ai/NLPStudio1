testDocument <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("en_US.twitter", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.twitter", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("en_US.news", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.news", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("en_US.blogs", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.blogs", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    DocumentTest <<- LogTest$new()
    file.copy('./test/testData/hc/en_US.blogs.txt', 'test/testData/en_US.blogs.txt')
    textData <- readLines(con = 'test/testData/en_US.blogs.txt')
    textData <- textData[1:2000]
    writeLines(textData, con = 'test/testData/en_US.blogs.txt')
    rm(textData)
  }

  test0 <- function() {
    test <- "test0: Document: Instantiation and Core Methods"
    cat(paste0("\n",test, " Commencing\n"))

    # Validation
    #blogs <- Document$new()# should fail -success
    #blogs <- Document$new('./test/usd.txxxt', io = 'hat') # should fail, file does not exist
    #blogs <- Document$new('./test/testData/hc/en_US.blogs.txt', io = 'car') # io Mismatch
    #blogs <- Document$new('./test/testData/hc/en_US.blogs.txt', io = IORdata$new) # io Mismatch

    # Instantiate
    blogs <- Document$new('./test/testData/en_US.blogs.txt', io = IOText$new(), desc = 'Blogs Register')
    d <- blogs$exposeObject()
    stopifnot(d$className == 'Document')
    stopifnot(d$name == 'en_US.blogs')
    stopifnot(d$fileName == 'en_US.blogs.txt')
    stopifnot(d$desc == 'Blogs Register')
    stopifnot(d$path == './test/testData/en_US.blogs.txt')
    stopifnot(isTRUE(all.equal(d$io, IOText$new())))
    stopifnot(!is.null(d$logs))
    stopifnot(blogs$getClassName() == "Document")
    stopifnot(blogs$getName() == "en_US.blogs")
    stopifnot(blogs$getFileName() == "en_US.blogs.txt")
    stopifnot(blogs$getPath() == "./test/testData/en_US.blogs.txt")
    stopifnot(isTRUE(all.equal(blogs$getIO(), IOText$new())))

    # Logit
    DocumentTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized document"))
    DocumentTest$logs(className = className, methodName = "getClassName", msg = paste("Successfully obtained class name"))
    DocumentTest$logs(className = className, methodName = "getName", msg = paste("Successfully obtained name"))
    DocumentTest$logs(className = className, methodName = "getFileName", msg = paste("Successfully obtained file name"))
    DocumentTest$logs(className = className, methodName = "getPath", msg = paste("Successfully obtained path"))
    DocumentTest$logs(className = className, methodName = "getIO", msg = paste("Successfully obtained IO"))
    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test1 <- function(blogs) {
    test <- "test1: Document: IO"
    cat(paste0("\n",test, " Commencing\n"))

    # Read content
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

    # WriteBin
    blogs$write(IOBin$new())
    b <- blogs$exposeObject()
    stopifnot((Sys.time() -  b$modified) < 1)

    # Write Archive
    blogs$write(IOArchive$new())
    b <- blogs$exposeObject()
    stopifnot((Sys.time() -  b$modified) < 1)

    # Read archive
    blogs <- blogs$read(IOArchive$new())
    content <- blogs$getContent()
    stopifnot(length(content) > 1000)


    # Logit
    DocumentTest$logs(className = className, methodName = "read", msg = paste("Successfully read the document in txt format"))
    DocumentTest$logs(className = className, methodName = "read", msg = paste("Successfully read the document in bin format"))
    DocumentTest$logs(className = className, methodName = "read", msg = paste("Successfully read the document in archive (bzip2) format"))
    DocumentTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote the document in txt format"))
    DocumentTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote the document in bin format"))
    DocumentTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote the document in archive (bzip2) format"))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

init()
blogs <<- test0()
blogs <<- test1(blogs)


}
className <- "Document"
#source('./test/unitTests/testCorpusBuilder.R')
testDocument()
