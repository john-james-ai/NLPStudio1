testCorpusSourceQuanteda <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusSourceQuantedaTest <<- LogTest$new()
    qc <- quanteda::corpus(readtext::readtext("./test/testData/input"))
    quanteda::docvars(qc, "name") <- c("Blogs", "News", "Twitter")
    quanteda::docvars(qc, "year") <- "2018"
    return(qc)
  }

  test0 <- function(qc) {
    test <- "test0: CorpusSourceQuanteda"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from directory source
    name <- "CorpusSourceQuanteda"
    desc <- "Creating corpus from Quanteda corpus object"
    docDesc <- c("Blogs Data", "News of the world", "Tweets and rants")


    # Validation
    #corpus <- CorpusSourceQuanteda$new() # missing params
    #corpus <- CorpusSourceQuanteda$new("foo") # missing param data source
    #corpus <- CorpusSourceQuanteda$new(222, "dataSource") # invalid name
    #corpus <- CorpusSourceQuanteda$new("foo bar", "dataSource") # invalid name
    #corpus <- CorpusSourceQuanteda$new(TRUE, "dataSource") # invalid name
    #corpus <- CorpusSourceQuanteda$new(Entity, "dataSource") # invalid name
    #corpus <- CorpusSourceQuanteda$new(222, "dataSource") # invalid name
    # corpus <- CorpusSourceQuanteda$new(newsTxt, "dataSource") # invalid name
    # corpus <- CorpusSourceQuanteda$new(start, "dataSource") # invalid name
    #corpus <- CorpusSourceQuanteda$new(name, 22) # invalid data source
    #corpus <- CorpusSourceQuanteda$new(name, TRUE) # invalid data source

    dataSource <- qc
    corpus <- CorpusSourceQuanteda$new(name, dataSource)$build()$getResult()
    content <- corpus$read()
    stopifnot(length(content) == 3)

    docs <- corpus$getDocuments()
    stopifnot(length(docs) == 3)
    corpus$meta(key = "desc", value = desc)
    corpus$docMeta(key = "desc", value = docDesc)
    print(corpus$meta())
    print(corpus$docMeta())

    CorpusSourceQuantedaTest$logs(className = "CorpusSourceQuanteda", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusSourceQuantedaTest$logs(className = "CorpusSourceQuanteda", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusSourceQuantedaTest$logs(className = "CorpusSourceQuanteda", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: CorpusSourceQuanteda: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    CorpusSourceQuantedaTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }

qc <- init()
test0(qc)


}
className <- "CorpusSourceQuanteda"
#source('./test/unitTests/testCorpusSourceQuanteda.R')
testCorpusSourceQuanteda()
