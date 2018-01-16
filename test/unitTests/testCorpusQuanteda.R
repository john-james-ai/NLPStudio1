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
    dataSource <- qc
    CorpusImportQuanteda <- CorpusImportQuanteda$new(name, dataSource)$build()$getResult()
    CorpusImportQuantedaContent <- CorpusImportQuanteda$read()
    stopifnot(length(CorpusImportQuantedaContent) == 3)

    CorpusImportQuantedaDocuments <- CorpusImportQuanteda$getDocuments()
    stopifnot(length(CorpusImportQuantedaDocuments) == 3)
    CorpusImportQuanteda$meta(key = "desc", value = desc)
    CorpusImportQuanteda$docMeta(key = "desc", value = docDesc)
    print(CorpusImportQuanteda$meta())
    print(CorpusImportQuanteda$docMeta())

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
