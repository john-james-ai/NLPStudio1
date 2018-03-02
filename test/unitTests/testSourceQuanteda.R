testSourceQuanteda <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    SourceQuantedaTest <<- LogTest$new()
    qc <- quanteda::corpus(readtext::readtext("./test/testData/input"))
    quanteda::docvars(qc, "name") <- c("Blogs", "News", "Twitter")
    quanteda::docvars(qc, "year") <- "2018"
    return(qc)
  }

  test0 <- function(qc) {
    test <- "test0: SourceQuanteda"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from directory source
    name <- "SourceQuanteda"
    desc <- "Creating corpus from Quanteda corpus object"
    docDesc <- c("Blogs Data", "News of the world", "Tweets and rants")


    # Validation
    #corpus <- SourceQuanteda$new() # missing params
    #corpus <- SourceQuanteda$new("foo") # missing param data source
    #corpus <- SourceQuanteda$new(222, "corpusSource") # invalid name
    #corpus <- SourceQuanteda$new("foo bar", "corpusSource") # invalid name
    #corpus <- SourceQuanteda$new(TRUE, "corpusSource") # invalid name
    #corpus <- SourceQuanteda$new(Entity, "corpusSource") # invalid name
    #corpus <- SourceQuanteda$new(222, "corpusSource") # invalid name
    # corpus <- SourceQuanteda$new(newsTxt, "corpusSource") # invalid name
    # corpus <- SourceQuanteda$new(start, "corpusSource") # invalid name
    #corpus <- SourceQuanteda$new(name, 22) # invalid data source
    #corpus <- SourceQuanteda$new(name, TRUE) # invalid data source

    corpusSource <- qc
    corpus <- SourceQuanteda$new(name, corpusSource)$build()$getResult()
    content <- corpus$read()
    stopifnot(length(content) == 3)

    docs <- corpus$getDocuments()
    stopifnot(length(docs) == 3)
    corpus$meta(key = "desc", value = desc)
    corpus$docMeta(key = "desc", value = docDesc)
    print(corpus$meta())
    print(corpus$docMeta())

    SourceQuantedaTest$logs(className = "SourceQuanteda", methodName = "initiate", msg = paste("Successfully instantiated. "))
    SourceQuantedaTest$logs(className = "SourceQuanteda", methodName = "build", msg = paste("Successfully instantiated. "))
    SourceQuantedaTest$logs(className = "SourceQuanteda", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: SourceQuanteda: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    SourceQuantedaTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }

qc <- init()
test0(qc)


}
className <- "SourceQuanteda"
#source('./test/unitTests/testSourceQuanteda.R')
testSourceQuanteda()
