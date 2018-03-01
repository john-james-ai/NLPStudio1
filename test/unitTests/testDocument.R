testDocument <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("blogs", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("blogs", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    blogsTxt <<- readLines("./test/testData/input/en_US.blogs.txt")
    newsTxt <<- readLines("./test/testData/input/en_US.news.txt")
    twitterTxt <<- readLines("./test/testData/input/en_US.twitter.txt")
    DocumentTest <<- LogTest$new()
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

    # Check meta data
    blogsMeta <- blogs$meta()
    stopifnot(blogsMeta$title == "Blogs")
    stopifnot(blogsMeta$subject == "Technology Blogs")
    stopifnot(blogsMeta$description == "Blogs about technology and business")
    stopifnot(blogsMeta$language == "en")
    stopifnot(blogsMeta$creator == "Hans Christensen")
    stopifnot(blogsMeta$dateCreated == "12/22/2014")
    stopifnot(blogsMeta$source == "www.hc.com")
    stopifnot(blogsMeta$format == "txt")

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
    blogs$text <- blogsTxt
    bc3 <- blogs$text
    blogsTxt <- blogs$read()
    stopifnot(blogsTxt == bc3)

    # Logit
    DocumentTest$logs(className = className, methodName = "content", msg = paste("Successfully added content to document"))
    DocumentTest$logs(className = className, methodName = "content", msg = paste("Successfully obtained content from document"))
    DocumentTest$logs(className = className, methodName = "setContent", msg = paste("Successfully added content to document"))
    DocumentTest$logs(className = className, methodName = "read", msg = paste("Successfully obtained content from document"))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test3 <- function(blogs) {
    test <- "Test3: Document: Read / Write File"
    cat(paste0("\n",test, " Commencing\n"))

    path <- "./test/testData/output"
    blogsTxtFileName <- "blogs.txt"
    blogsBinFileName <- "blogs.RData"
    blogsTxtFilePath <- file.path(path, blogsTxtFileName)
    blogsBinFilePath <- file.path(path, blogsBinFileName)

    # Read no path
    blogsTxt <<- blogs$read()
    stopifnot(length(blogsTxt) == 2000)
    print(head(blogsTxt), 2)

    # Write binary file
    blogs$text <- newsTxt
    blogs$write(blogsBinFilePath, io = IOBin$new())
    stopifnot(file.exists(blogsBinFilePath))

    # Write Text File
    blogs$write(blogsTxtFilePath)
    stopifnot(file.exists(blogsTxtFilePath))

    # Read Binary file
    blogsBinContent <- blogs$read(blogsBinFilePath, io = IOBin$new())
    stopifnot(length(blogsBinContent) > 2000)
    stopifnot(blogsBinContent == blogs$text)
    print(head(blogsBinContent), 2)

    # Read Txt File from File
    start <- Sys.time()
    blogsTxtContent <- blogs$read(blogsTxtFilePath)
    stopifnot(length(blogsTxtContent) == 2000)
    stopifnot(blogsTxtContent == blogs$text)
    end <- Sys.time()
    readTime1 <- difftime(end, start)
    print(paste("Readtime1: ", readTime1))

    # Read Txt Memory
    start <- Sys.time()
    blogsTxtContent <- blogs$read(blogsTxtFilePath)
    stopifnot(length(blogsTxtContent) == 2000)
    stopifnot(blogsTxtContent == blogs$text)
    end <- Sys.time()
    readTime2 <- difftime(end, start)
    print(paste("Readtime2: ", readTime2))
    stopifnot(readTime2 < readTime1)


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
