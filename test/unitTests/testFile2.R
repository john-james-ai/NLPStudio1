testFile <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("blogs", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("blogs", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    blogs <- readLines("./test/testData/hc/en_US.blogs.txt")
    blogs <- blogs[1:2000]
    writeLines(blogs, "./test/testData/input/en_US.blogs.txt")
    FileTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "Test0: Text File: Load and read"
    cat(paste0("\n",test, " Commencing\n"))

    # Instantiate
    blogs <- File$new(name, path)
    b <- blogs$exposeObject()
    stopifnot(b$name == name)
    stopifnot(b$path == path)
    stopifnot(b$fileName == "en_US.blogs.txt")
    stopifnot("IOText" %in% class(b$io))

    # Load file
    blogs <- blogs$loadFile()
    b <- blogs$exposeObject()
    stopifnot(b$fileSize > 1000)
    stopifnot(b$fileFormat == 'txt')
    stopifnot(length(b$text) > 1000)

    # Read file
    blogsContent <- blogs$read()
    stopifnot(length(blogsContent) > 1000)

    # Method chaining
    blogsContent <- File$new(name, path)$loadFile()$read()
    stopifnot(length(blogsContent) > 1000)

    # Logit
    FileTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized file"))
    FileTest$logs(className = className, methodName = "loadFile", msg = paste("Successfully loaded file"))
    FileTest$logs(className = className, methodName = "read", msg = paste("Successfully obtained content"))
    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }


  test1 <- function(blogs) {
    test <- "Test1: Text File: Set content and save"
    cat(paste0("\n",test, " Commencing\n"))

    # Update content
    blogsContent <- blogs$read()
    blogsContent <- blogsContent[1:1000]
    blogs <- blogs$setContent(blogsContent)
    blogsContent2 <- blogs$read()
    stopifnot(length(blogsContent2) == 1000)

    # Save and read content
    blogs <- blogs$saveFile()
    blogsContent3 <- blogs$loadFile()$read()
    stopifnot(length(blogsContent3) == 1000)

    # Logit
    FileTest$logs(className = className, methodName = "setContent", msg = paste("Successfully tested set content functionality"))
    FileTest$logs(className = className, methodName = "saveFile", msg = paste("Successfully saved file"))
    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test2 <- function(blogs) {
    test <- "Test2: Binary File: Read and Write"
    cat(paste0("\n",test, " Commencing\n"))

    # Read binary format
    io <- IOBin$new()
    blogsBin <- File$new(name, path)$loadFile(io)$read()
    stopifnot(class(blogsBin) == 'raw')

    # Write binary format
    blogsBin2 <- File$new(name, path)$setContent(blogsBin)$saveFile(io)
    b <- blogsBin2$exposeObject()
    stopifnot(class(b$text) == 'raw')

    # Logit
    FileTest$logs(className = className, methodName = "loadFile", msg = paste("Successfully loaded binary data"))
    FileTest$logs(className = className, methodName = "saveFile", msg = paste("Successfully saved binary data"))
    FileTest$logs(className = className, methodName = "read", msg = paste("Successfully read binary data"))
    FileTest$logs(className = className, methodName = "setContent", msg = paste("Successfully wrote binary data"))
    cat(paste0(test, " Completed: Success!\n"))

    return(blogsBin2)
  }

# Global Variables
  name = 'blogs'
  path = "./test/testData/input/en_US.blogs.txt"
  init()
  blogs <- test0()
  blogs <- test1(blogs)
  blogs <- test2(blogs)


}
className <- "File"
#source('./test/unitTests/testCorpusBuilder.R')
testFile()
