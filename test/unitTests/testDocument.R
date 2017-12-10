testDocument <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("en_US.twitter", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.twitter", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("en_US.news", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.news", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("en_US.blogs", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.blogs", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    DocumentTest <<- LogTest$new()
    file.copy('./test/testData/hc/en_US.blogs.txt', 'test/testData/en_US.blogs.txt')
    textData <- readLines(con = 'test/testData/en_US.blogs.txt')
    writeLines(textData[1:2000], con = 'test/testData/en_US.blogs.txt')
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

    # WriteBin
    blogs$write(IOBin$new())
    b <- blogs$exposeObject()
    stopifnot((Sys.time() -  b$modified) < 1)


    # Logit
    DocumentTest$logs(className = className, methodName = "read", msg = paste("Successfully read the document in txt format"))
    DocumentTest$logs(className = className, methodName = "read", msg = paste("Successfully read the document in bin format"))
    DocumentTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote the document in txt format"))
    DocumentTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote the document in bin format"))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test2 <- function(blogs) {
    test <- "test2: Document: Refine"
    cat(paste0("\n",test, " Commencing\n"))

    blogs <- blogs$refine()
    b <- blogs$exposeObject()
    stopifnot((Sys.time() -  b$modified) < 1)
    stopifnot(b$name == 'en_US.blogs')
    stopifnot(b$fileName == 'en_US.blogs.Rdata')
    stopifnot(b$path == './test/testData/en_US.blogs.Rdata')
    file.remove('./test/testData/en_US.blogs.txt')

    content <- blogs$getContent()
    stopifnot(length(content) > 1000)

    # Logit
    DocumentTest$logs(className = className, methodName = "refine", msg = paste("Successfully refined data and stored in rdata format."))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }


  test3 <- function(blogs) {
    test <- "test3: Document: Parse"
    cat(paste0("\n",test, " Commencing\n"))

    # Run fast parser
    blogs <- blogs$parseFast(numbers = TRUE, punct = TRUE,
                    symbols = TRUE,  twitter = TRUE, hyphens = TRUE,
                    url = TRUE)
    b <- blogs$exposeObject()
    stopifnot((Sys.time() -  b$modified) < 1)


    # Run full parser
    blogs <- blogs$parseFull(numbers = TRUE, punct = TRUE,
                    symbols = TRUE,  twitter = TRUE, hyphens = TRUE,
                      url = TRUE, email = TRUE, control = FALSE,
                    repeatChars = TRUE, longWords = TRUE)

    b <- blogs$exposeObject()
    stopifnot((Sys.time() -  b$modified) < 1)

    # Logit
    DocumentTest$logs(className = className, methodName = "parseFast", msg = paste("Successfully parsed", blogs$getName(), "with parseFast."))
    DocumentTest$logs(className = className, methodName = "parseFull", msg = paste("Successfully parsed", blogs$getName(), "with parseFull."))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }



  testn <- function(blogs) {
    test <- "testn: Document: IO"
    cat(paste0("\n",test, " Commencing\n"))

        # Logit
    DocumentTest$logs(className = className, methodName = "read", msg = paste("Successfully read the document in txt format"))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

init()
blogs <<- test0()
blogs <<- test1(blogs)
blogs <<- test2(blogs)
blogs <<- test3(blogs)
}
className <- "Document"
#source('./test/unitTests/testCorpusBuilder.R')
testDocument()
