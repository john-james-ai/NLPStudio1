testCorpusImportQuanteda <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusImportQuantedaTest <<- LogTest$new()
    qc <- quanteda::corpus(readtext::readtext("./test/testData/input"))
    quanteda::docvars(qc, "name") <- c("Blogs", "News", "Twitter")
    quanteda::docvars(qc, "year") <- "2018"
    return(qc)
  }

  test0 <- function(qc) {
    test <- "test0: CorpusImportQuanteda"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from directory source
    name <- "CorpusImportQuanteda"
    desc <- "Creating corpus from Quanteda corpus object"
    docDesc <- c("Blogs Data", "News of the world", "Tweets and rants")


    # Validation
    #corpus <- CorpusImportQuanteda$new() # missing params
    #corpus <- CorpusImportQuanteda$new("foo") # missing param data source
    #corpus <- CorpusImportQuanteda$new(222, "dataSource") # invalid name
    #corpus <- CorpusImportQuanteda$new("foo bar", "dataSource") # invalid name
    #corpus <- CorpusImportQuanteda$new(TRUE, "dataSource") # invalid name
    #corpus <- CorpusImportQuanteda$new(Entity, "dataSource") # invalid name
    #corpus <- CorpusImportQuanteda$new(222, "dataSource") # invalid name
    # corpus <- CorpusImportQuanteda$new(newsTxt, "dataSource") # invalid name
    # corpus <- CorpusImportQuanteda$new(start, "dataSource") # invalid name
    #corpus <- CorpusImportQuanteda$new(name, 22) # invalid data source
    #corpus <- CorpusImportQuanteda$new(name, TRUE) # invalid data source

    dataSource <- qc
    corpus <- CorpusImportQuanteda$new(name, dataSource)$build()$getResult()
    content <- corpus$read()
    stopifnot(length(content) == 3)

    docs <- corpus$getDocuments()
    stopifnot(length(docs) == 3)
    corpus$meta(key = "desc", value = desc)
    corpus$docMeta(key = "desc", value = docDesc)
    print(corpus$meta())
    print(corpus$docMeta())

    CorpusImportQuantedaTest$logs(className = "CorpusImportQuanteda", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusImportQuantedaTest$logs(className = "CorpusImportQuanteda", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusImportQuantedaTest$logs(className = "CorpusImportQuanteda", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: CorpusImportQuanteda: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    CorpusImportQuantedaTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }

qc <- init()
test0(qc)


}
className <- "CorpusImportQuanteda"
#source('./test/unitTests/testCorpusImportQuanteda.R')
testCorpusImportQuanteda()
