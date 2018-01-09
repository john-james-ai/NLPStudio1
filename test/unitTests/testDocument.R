testDocument <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("blogs", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("blogs", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    blogTxt <- readLines("./test/testData/hc/en_US.blogs.txt")
    newsTxt <- readLines("./test/testData/hc/en_US.news.txt")
    twitterTxt <- readLines("./test/testData/hc/en_US.twitter.txt")
    DocumentTest <- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: Document: Instantiation document no file"
    cat(paste0("\n",test, " Commencing\n"))

    # Validation
    # blogs <- Document$new()# should fail, no name

    # Instantiate
    blogs <- Document$new(name = 'blogs')
    d <- blogs$exposeObject()
    stopifnot(d$name == 'blogs')

    # Logit
    DocumentTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized document"))
    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test1 <- function(blogs) {
    test <- "test1: Document: Read/Write Metadata"
    cat(paste0("\n",test, " Commencing\n"))

    blogs <- blogs$meta(key = "title", value = "Blogs")
    blogs <- blogs$meta(key = "subject", value = "Technology Blogs")
    blogs <- blogs$meta(key = "description", value = "Blogs about technology and business")
    blogs <- blogs$meta(key = "language", value = "en")
    blogs <- blogs$meta(key = "creator", value = "Hans Christensen")
    blogs <- blogs$meta(key = "dateCreated", value = "12/22/2014")
    blogs <- blogs$meta(key = "source", value = "www.hc.com")
    blogs <- blogs$meta(key = "format", value = "txt")


    # Test
    print(blogs$meta())

    # Logit
    DocumentTest$logs(className = className, methodName = "metadata", msg = paste("Successfully updated meta data on a document"))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test2 <- function(blogs) {
    test <- "test2: Document: Get / Set  content"
    cat(paste0("\n",test, " Commencing\n"))

    # Add content via active binding
    blogs$content <- blogTxt
    bc3 <- blogs$content
    stopifnot(blogTxt == bc3)

    # Add content via method
    blogs <- blogs$setContent(newsTxt)
    bb <- blogs$getContent()
    stopifnot(bb == newsTxt)

    # Logit
    DocumentTest$logs(className = className, methodName = "content", msg = paste("Successfully added content to document"))
    DocumentTest$logs(className = className, methodName = "content", msg = paste("Successfully obtained content from document"))
    DocumentTest$logs(className = className, methodName = "setContent", msg = paste("Successfully added content to document"))
    DocumentTest$logs(className = className, methodName = "getContent", msg = paste("Successfully obtained content from document"))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test3 <- function(blogs) {
    test <- "Test3: Document: Read File"
    cat(paste0("\n",test, " Commencing\n"))

    blogsTxt <- blogs$read(path = './test/testData/hc/en_US.blogs.txt')
    blogsBin <- blogs$read(path = './test/testData/hc/en_US.blogs.txt', io = IOBin$new())

    # Logit
    DocumentTest$logs(className = className, methodName = "content", msg = paste("Successfully instantiated document with file"))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test4 <- function(blogs) {
    test <- "Test4: Document: Write File"
    cat(paste0("\n",test, " Commencing\n"))

    blogs1 <- blogs$write(path = './test/testData/output/en_US.blogs.txt', content = blogTxt)
    blogs2 <- blogs$write(path = './test/testData/hc/en_US.blogs.bin', io = IOBin$new(), content = blogsBin)

    # Logit
    DocumentTest$logs(className = className, methodName = "content", msg = paste("Successfully instantiated document with file"))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }


init()
blogs <- test0()
blogs <<- test1(blogs)
blogs <<- test2(blogs)
blogs <<- test3(blogs)

}
className <- "Document"

testDocument()
