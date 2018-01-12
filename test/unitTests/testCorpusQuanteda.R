testCorpusQuanteda <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusQuantedaTest <<- LogTest$new()
    qc <- quanteda::corpus(readtext::readtext("./test/testData/input"))
    quanteda::docvars(qc, "name") <- c("Blogs", "News", "Twitter")
    quanteda::docvars(qc, "year") <- "2018"
    return(qc)
  }

  test0 <- function(qc) {
    test <- "test0: CorpusQuanteda"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from directory source
    name <- "corpusQuanteda"
    desc <- "Creating corpus from Quanteda corpus object"
    docDesc <- c("Blogs Data", "News of the world", "Tweets and rants")
    dataSource <- qc
    corpusQuanteda <- CorpusQuanteda$new(name, dataSource)$build()$getResult()
    corpusQuantedaContent <- corpusQuanteda$getContent()
    stopifnot(length(corpusQuantedaContent) == 3)

    corpusQuantedaDocuments <- corpusQuanteda$getDocuments()
    stopifnot(length(corpusQuantedaDocuments) == 3)
    corpusQuanteda$meta(key = "desc", value = desc)
    corpusQuanteda$docMeta(key = "desc", value = docDesc)
    print(corpusQuanteda$meta())
    print(corpusQuanteda$docMeta())

    CorpusQuantedaTest$logs(className = "CorpusQuanteda", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusQuantedaTest$logs(className = "CorpusQuanteda", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusQuantedaTest$logs(className = "CorpusQuanteda", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: CorpusQuanteda: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    CorpusQuantedaTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }

qc <- init()
test0(qc)


}
className <- "CorpusQuanteda"
#source('./test/unitTests/testCorpusQuanteda.R')
testCorpusQuanteda()
