testCloneDocument <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("blogs", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("blogs", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    blogsTxt <<- readLines("./test/testData/input/en_US.blogs.txt")
    newsTxt <<- readLines("./test/testData/input/en_US.news.txt")
    CloneDocumentTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: Document: Instantiation document no file"
    cat(paste0("\n",test, " Commencing\n"))

    # Validation
    # blogs <- Document$new()# should fail, no name

    # Instantiate
    blogs <- Document$new(name = 'blogs')
    blogs$text <- blogsTxt
    d <- blogs$exposeObject()
    stopifnot(d$name == 'blogs')

    # Logit
    CloneDocumentTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized document"))
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
    CloneDocumentTest$logs(className = className, methodName = "metadata", msg = paste("Successfully updated meta data on a document"))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test2 <- function(blogs) {
    test <- "test2: Clone meta data"
    cat(paste0("\n",test, " Commencing\n"))

    # Add content via active binding
    blogs2 <- Document$new(name = 'blogs2')
    d <- blogs2$exposeObject()
    stopifnot(d$name == 'blogs2')

    # Clone meta data
    keys <- names(as.list(blogs$meta()))
    keys <- keys[keys!= "name"]
    values <- as.list(blogs$meta())
    values["name"] <- NULL
    blogs2$text <- blogs$text
    lapply(seq_along(keys), function(k) {
      blogs2$meta(key = keys[[k]], value = values[[k]])
    })

    meta1 <- blogs2$meta()
    meta2 <- blogs$meta()
    content1 <- blogs$text
    content2 <- blogs2$text
    stopifnot(identical(meta1, meta2))
    stopifnot(identical(content1, content2))

    # Logit
    CloneDocumentTest$logs(className = className, methodName = "cloneDocument", msg = paste("Successfully cloned to document"))
    CloneDocumentTest$logs(className = className, methodName = "content", msg = paste("Successfully obtained content from document"))
    CloneDocumentTest$logs(className = className, methodName = "setContent", msg = paste("Successfully added content to document"))
    CloneDocumentTest$logs(className = className, methodName = "read", msg = paste("Successfully obtained content from document"))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  testn <- function(blogs) {
    test <- "Testn: Document: Write File"
    cat(paste0("\n",test, " Commencing\n"))

    # Logit
    CloneDocumentTest$logs(className = className, methodName = "content", msg = paste("Successfully instantiated document with file"))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }


init()
blogs <- test0()
blogs <<- test1(blogs)
blogs <<- test2(blogs)


}
className <- "Document"

testCloneDocument()
