testCorpusImportDir <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusImportDirTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: CorpusImportDir: Directory"
    cat(paste0("\n",test, " Commencing\n"))

    # Init params
    name <- "corpus"
    desc <- "Creating corpus from directory sources"
    dataSource <- "./test/testData/input"

    # Validation
    #corpus <- CorpusImportDir$new() # missing params
    #corpus <- CorpusImportDir$new("foo") # missing param data source
    #corpus <- CorpusImportDir$new(222, "dataSource") # invalid name
    #corpus <- CorpusImportDir$new("foo bar", "dataSource") # invalid name
    #corpus <- CorpusImportDir$new(TRUE, "dataSource") # invalid name
    #corpus <- CorpusImportDir$new(Entity, "dataSource") # invalid name
    #corpus <- CorpusImportDir$new(222, "dataSource") # invalid name
    #corpus <- CorpusImportDir$new(newsTxt, "dataSource") # invalid name
    #corpus <- CorpusImportDir$new(start, "dataSource") # invalid name
    #corpus <- CorpusImportDir$new(name, 22) # invalid data source
    #corpus <- CorpusImportDir$new(name, TRUE) # invalid data source
    corpus <- CorpusImportDir$new(name, "test/testfoo")$build()$getResult() # invalid data source

    # Build Corpus from directory source
    corpus <- CorpusImportDir$new(name, dataSource)$build()$getResult()
    corpusContent <- corpus$read()
    stopifnot(length(corpusContent) == 3)
    docs <- corpus$getDocuments()
    stopifnot(length(docs) == 3)
    corpus$meta(key = "desc", value = desc)
    corpus$docMeta(key = "year", value = "2018")
    print(corpus$meta())
    print(corpus$docMeta())

    CorpusImportDirTest$logs(className = "CorpusImportDir", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusImportDirTest$logs(className = "CorpusImportDir", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusImportDirTest$logs(className = "CorpusImportDir", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(corpus)
  }

  test1 <- function() {
    test <- "test1: CorpusImportDir: WildCard"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from Vector Flat
    name <- "CorpusImportDir"
    desc <- "Creating corpus from directory sources"
    dataSource <- "./test/testData/input/*s.txt"
    corpus <- CorpusImportDir$new(name, dataSource)$build()$getResult()
    corpusContent <- corpus$read()
    stopifnot(length(corpusContent) == 2)
    docs <- corpus$getDocuments()
    stopifnot(length(docs) == 2)
    corpus$meta(key = "desc", value = desc)
    corpus$docMeta(key = "year", value = "2018")
    print(corpus$meta())
    print(corpus$docMeta())

    CorpusImportDirTest$logs(className = "CorpusImportDir", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusImportDirTest$logs(className = "CorpusImportDir", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusImportDirTest$logs(className = "CorpusImportDir", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: CorpusImportDir: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    CorpusImportDirTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
corpus0 <- test0()
corpus1 <- test1()


}
className <- "CorpusImportDir"
#source('./test/unitTests/testCorpusImportDir.R')
testCorpusImportDir()
