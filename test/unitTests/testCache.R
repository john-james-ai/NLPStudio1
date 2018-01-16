testCorpusImportDir <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusImportDirTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: CorpusImportDir: Directory"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from directory source
    name <- "CorpusImportDir"
    desc <- "Creating corpus from directory sources"
    dataSource <- "./test/testData/input"
    CorpusImportDir <- CorpusImportDir$new(name, dataSource)$build()$getResult()
    CorpusImportDirContent <- CorpusImportDir$read()
    stopifnot(length(CorpusImportDirContent) == 3)
    CorpusImportDirDocuments <- CorpusImportDir$getDocuments()
    stopifnot(length(CorpusImportDirDocuments) == 3)
    CorpusImportDir$meta(key = "desc", value = desc)
    CorpusImportDir$docMeta(key = "year", value = "2018")
    print(CorpusImportDir$meta())
    print(CorpusImportDir$docMeta())

    CorpusImportDirTest$logs(className = "CorpusImportDir", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusImportDirTest$logs(className = "CorpusImportDir", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusImportDirTest$logs(className = "CorpusImportDir", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }

  test1 <- function() {
    test <- "test1: CorpusImportDir: WildCard"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from Vector Flat
    name <- "CorpusImportDir"
    desc <- "Creating corpus from directory sources"
    dataSource <- "./test/testData/input/*s.txt"
    CorpusImportDir <- CorpusImportDir$new(name, dataSource)$build()$getResult()
    CorpusImportDirContent <- CorpusImportDir$read()
    stopifnot(length(CorpusImportDirContent) == 2)
    CorpusImportDirDocuments <- CorpusImportDir$getDocuments()
    stopifnot(length(CorpusImportDirDocuments) == 2)
    CorpusImportDir$meta(key = "desc", value = desc)
    CorpusImportDir$docMeta(key = "year", value = "2018")
    print(CorpusImportDir$meta())
    print(CorpusImportDir$docMeta())

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
test0()
test1()


}
className <- "CorpusImportDir"
#source('./test/unitTests/testCorpusImportDir.R')
testCorpusImportDir()
