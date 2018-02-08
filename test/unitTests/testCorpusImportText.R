testCorpusImportText <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusImportTextTest <<- LogTest$new()
    files <- list.files(path = "./test/testData/input", full.names = TRUE)
    corpus <<- lapply(files, function(f) {
      readLines(f)
    })


  }

  test0 <- function() {
    test <- "test0: CorpusImportText"
    cat(paste0("\n",test, " Commencing\n"))

    name <- "corpusVecFlat"

    # Validation
    #corpus <- CorpusImportText$new() # missing params
    #corpus <- CorpusImportText$new("foo") # missing param data source
    corpus <- CorpusImportText$new("foo", "dataSource") # invalid name

    # Build Corpus from Vector Flat
    news <- readLines("./test/testData/input/en_US.news.txt")
    dataSource <- news
    corpusVecFlat <- CorpusImportText$new(name, dataSource, flat = TRUE)$build()$getResult()
    corpusVecFlatContent <- corpusVecFlat$read()
    stopifnot(length(corpusVecFlatContent$corpusVec) == 2000)
    stopifnot(corpusVecFlatContent$corpusVec == news)
    corpusVecFlatDocuments <- corpusVecFlat$getDocuments()
    stopifnot(length(corpusVecFlatDocuments) == 1)
    print(corpusVecFlat$meta())
    print(corpusVecFlat$docMeta())

    # Build Corpus from vector non-flat
    name <- "corpusVec"
    qc <- quanteda::corpus(readtext::readtext("./test/testData/input/*.txt"))
    dataSource <- qc$documents$texts
    corpusVec <- CorpusImportText$new(name, dataSource)$build()$getResult()
    corpusVecContent <- corpusVec$read()
    stopifnot(length(corpusVecContent) == 3)
    corpusVecDocuments <- corpusVec$getDocuments()
    stopifnot(length(corpusVecDocuments) == 3)
    print(corpusVec$meta())
    print(corpusVec$docMeta())

    # Build Corpus from list
    name <- "corpusList"
    dataSource <- corpusList
    corpusList <- CorpusImportText$new(name, dataSource)$build()$getResult()
    corpusListContent <- corpusList$read()
    stopifnot(length(corpusListContent) == 3)
    corpusListDocuments <- corpusList$getDocuments()
    stopifnot(length(corpusListDocuments) == 3)
    print(corpusList$meta())
    print(corpusList$docMeta())

    CorpusImportTextTest$logs(className = "CorpusImportText", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusImportTextTest$logs(className = "CorpusImportText", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusImportTextTest$logs(className = "CorpusImportText", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: CorpusImportText: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    CorpusImportTextTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
raw <- test0()



}
className <- "CorpusImportText"
#source('./test/unitTests/testCorpusImportText.R')
testCorpusImportText()
