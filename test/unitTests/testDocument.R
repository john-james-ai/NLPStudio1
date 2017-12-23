testDocument <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("newz", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("newz", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    DocumentTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: Document: Instantiation document no file"
    cat(paste0("\n",test, " Commencing\n"))

    # Validation
    # newz <- Document$new()# should fail, no name

    # Instantiate
    newz <- Document$new(name = 'newz')
    d <- newz$exposeObject()
    stopifnot(d$admin$name == 'newz')

    # Logit
    DocumentTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized document"))
    cat(paste0(test, " Completed: Success!\n"))

    return(newz)
  }

  test1 <- function(newz) {
    test <- "test1: Document: Read/Write Metadata"
    cat(paste0("\n",test, " Commencing\n"))

    # Assign
    newz$contributor <- "Joe Jons"
    newz$coverage <- "USA"
    newz$creator <- "Deff Freadley"
    newz$date <- "12/22/2017"
    newz$description <- "new corpus"
    newz$format <- "rdata"
    newz$identifier <- "232"
    newz$language <- "en"
    newz$publisher <- "house"
    newz$relation <- "sis"
    newz$rights <- "none"
    newz$source <- "raw"
    newz$subject <- "newz"
    newz$title <- "News Corpus"
    newz$type <- "corpus"

    # Test
    n <- newz$exposeObject()
    stopifnot(n$dublincore$contributor   =="Joe Jons")
    stopifnot(n$dublincore$coverage   =="USA")
    stopifnot(n$dublincore$creator   =="Deff Freadley")
    stopifnot(n$dublincore$date   =="12/22/2017")
    stopifnot(n$dublincore$description   =="new corpus")
    stopifnot(n$dublincore$format   =="rdata")
    stopifnot(n$dublincore$identifier   =="232")
    stopifnot(n$dublincore$language   =="en")
    stopifnot(n$dublincore$publisher   =="house")
    stopifnot(n$dublincore$relation   =="sis")
    stopifnot(n$dublincore$rights   =="none")
    stopifnot(n$dublincore$source   =="raw")
    stopifnot(n$dublincore$subject   =="newz")
    stopifnot(n$dublincore$title   =="News Corpus")
    stopifnot(n$dublincore$type   =="corpus")

    # Logit
    DocumentTest$logs(className = className, methodName = "metadata", msg = paste("Successfully updated meta data on a document"))

    cat(paste0(test, " Completed: Success!\n"))

    return(newz)
  }

  test2 <- function(newz) {
    test <- "test2: Document: Add content"
    cat(paste0("\n",test, " Commencing\n"))

    newz$content <- file
    stopifnot(length(newz$content) > 1000)

    # Logit
    DocumentTest$logs(className = className, methodName = "content", msg = paste("Successfully added content to document"))

    cat(paste0(test, " Completed: Success!\n"))

    return(newz)
  }

  test3 <- function(newz) {
    test <- "test3: Document: Create with file"
    cat(paste0("\n",test, " Commencing\n"))

    rm(newz)
    daller <- Document$new(name = 'daller', file = txtFile)
    stopifnot(length(newz$content) > 1000)

    # Logit
    DocumentTest$logs(className = className, methodName = "content", msg = paste("Successfully instantiated document with file"))

    cat(paste0(test, " Completed: Success!\n"))

    return(newz)
  }


init()
newz <- test0()
newz <<- test1(newz)
newz <<- test2(newz)
newz <<- test3(newz)

}
className <- "Document"

testDocument()
