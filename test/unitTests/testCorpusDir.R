testCorpusDir <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusDirTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: CorpusDir: Directory"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from directory source
    name <- "corpusDir"
    desc <- "Creating corpus from directory sources"
    dataSource <- "./test/testData/input"
    corpusDir <- CorpusDir$new(name, dataSource)$build()$getResult()
    corpusDirContent <- corpusDir$getContent()
    stopifnot(length(corpusDirContent) == 3)
    corpusDirDocuments <- corpusDir$getDocuments()
    stopifnot(length(corpusDirDocuments) == 3)
    corpusDir$meta(key = "desc", value = desc)
    corpusDir$docMeta(key = "year", value = "2018")
    print(corpusDir$meta())
    print(corpusDir$docMeta())

    CorpusDirTest$logs(className = "CorpusDir", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusDirTest$logs(className = "CorpusDir", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusDirTest$logs(className = "CorpusDir", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }

  test1 <- function() {
    test <- "test1: CorpusDir: WildCard"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from Vector Flat
    name <- "corpusDir"
    desc <- "Creating corpus from directory sources"
    dataSource <- "./test/testData/input/*s.txt"
    corpusDir <- CorpusDir$new(name, dataSource)$build()$getResult()
    corpusDirContent <- corpusDir$getContent()
    stopifnot(length(corpusDirContent) == 2)
    corpusDirDocuments <- corpusDir$getDocuments()
    stopifnot(length(corpusDirDocuments) == 2)
    corpusDir$meta(key = "desc", value = desc)
    corpusDir$docMeta(key = "year", value = "2018")
    print(corpusDir$meta())
    print(corpusDir$docMeta())

    CorpusDirTest$logs(className = "CorpusDir", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusDirTest$logs(className = "CorpusDir", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusDirTest$logs(className = "CorpusDir", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: CorpusDir: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    CorpusDirTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
test0()
test1()


}
className <- "CorpusDir"
#source('./test/unitTests/testCorpusDir.R')
testCorpusDir()
