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

    # Initialize
    contributor   <-   "Joe Jons"
    coverage   <-   "USA"
    creator   <-   "Deff Freadley"
    date   <-   "2/14/2016"
    description   <-   "new corpus"
    format   <-   "rdata"
    identifier   <-   "232"
    language   <-   "en"
    publisher   <-   "house"
    relation   <-   "sis"
    rights   <-   "none"
    source   <-   "raw"
    subject   <-   "newz"
    title   <-   "News Corpus"
    type   <-   "corpus"

    # Assign
    newz$contributor   <-   "Joe Jons"
    newz$coverage   <-   "USA"
    newz$creator   <-   "Deff Freadley"
    newz$date   <-   "43101"
    newz$description   <-   "new corpus"
    newz$format   <-   "rdata"
    newz$identifier   <-   "232"
    newz$language   <-   "en"
    newz$publisher   <-   "house"
    newz$relation   <-   "sis"
    newz$rights   <-   "none"
    newz$source   <-   "raw"
    newz$subject   <-   "newz"
    newz$title   <-   "News Corpus"
    newz$type   <-   "corpus"

    # Test
    stopifnot(newz$contributor   =="Joe Jons")
    stopifnot(newz$coverage   =="USA")
    stopifnot(newz$creator   =="Deff Freadley")
    stopifnot(newz$date   =="43101")
    stopifnot(newz$description   =="new corpus")
    stopifnot(newz$format   =="rdata")
    stopifnot(newz$identifier   =="232")
    stopifnot(newz$language   =="en")
    stopifnot(newz$publisher   =="house")
    stopifnot(newz$relation   =="sis")
    stopifnot(newz$rights   =="none")
    stopifnot(newz$source   =="raw")
    stopifnot(newz$subject   =="newz")
    stopifnot(newz$title   =="News Corpus")
    stopifnot(newz$type   =="corpus")

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

  test4 <- function(newz) {
    test <- "test4: Document: Create with file"
    cat(paste0("\n",test, " Commencing\n"))

    n <- newz$exposeObject()

    stopifnot(length(newz$content) > 1000)

    # Logit
    DocumentTest$logs(className = className, methodName = "content", msg = paste("Successfully instantiated document with file"))

    cat(paste0(test, " Completed: Success!\n"))

    return(n)
  }

init()
newz <- test0()
newz <<- test1(newz)
newz <<- test2(newz)
newz <<- test3(newz)
n <<- test4(newz)

}
className <- "Document"

testDocument()
