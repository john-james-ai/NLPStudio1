testDocument <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("blogs", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("blogs", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
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

    blogs <- blogs$docMeta(key = "contributor", value = "Joe Jons")
    blogs <- blogs$docMeta(key = "coverage", value = "USA")
    blogs <- blogs$docMeta(key = "creator", value = "Deff Freadley")
    blogs <- blogs$docMeta(key = "date", value = "43091")
    blogs <- blogs$docMeta(key = "description", value = "Blogs Document")
    blogs <- blogs$docMeta(key = "format", value = "txt")
    blogs <- blogs$docMeta(key = "identifier", value = "232")
    blogs <- blogs$docMeta(key = "language", value = "en")
    blogs <- blogs$docMeta(key = "publisher", value = "house")
    blogs <- blogs$docMeta(key = "relation", value = "sis")
    blogs <- blogs$docMeta(key = "rights", value = "none")
    blogs <- blogs$docMeta(key = "source", value = "hc")
    blogs <- blogs$docMeta(key = "subject", value = "blogs")
    blogs <- blogs$docMeta(key = "title", value = "Blogs Document")
    blogs <- blogs$docMeta(key = "type", value = "document")

    # Test
    print(blogs$docMeta())

    # Logit
    DocumentTest$logs(className = className, methodName = "metadata", msg = paste("Successfully updated meta data on a document"))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test2 <- function(blogs) {
    test <- "test2: Document: Get / Set  content"
    cat(paste0("\n",test, " Commencing\n"))

    # Add content via active binding
    blogs$content <- blogsContent2
    bc3 <- blogs$content
    stopifnot(blogsContent2 == bc3)

    # Add content via method
    blogs <- blogs$setContent(blogsBin)
    bb <- blogs$getContent()
    stopifnot(bb == blogsBin)


    # Write Content
    blogs <- blogs$write()

    # Read content
    bContent <- blogs$read()

    # Logit
    DocumentTest$logs(className = className, methodName = "content", msg = paste("Successfully added content to document"))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test3 <- function(blogs) {
    test <- "Test3: Document: Read File"
    cat(paste0("\n",test, " Commencing\n"))

    rm(blogs)
    daller <- Document$new(name = 'daller', file = txtFile)
    stopifnot(length(blogs$content) > 1000)

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
