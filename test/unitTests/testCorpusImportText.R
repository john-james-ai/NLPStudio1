testCorpusSourceText <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusSourceTextTest <<- LogTest$new()
    files <- list.files(path = "./test/testData/input", full.names = TRUE)
    corpus <<- lapply(files, function(f) {
      readLines(f)
    })


  }

  test0 <- function() {
    test <- "test0: CorpusSourceText"
    cat(paste0("\n",test, " Commencing\n"))

    name <- "corpusVecFlat"

    # Validation
    #corpus <- CorpusSourceText$new() # missing params
    #corpus <- CorpusSourceText$new("foo") # missing param data source
    corpus <- CorpusSourceText$new("foo", "dataSource") # invalid name

    # Build Corpus from Vector Flat
    news <- readLines("./test/testData/input/en_US.news.txt")
    dataSource <- news
    corpusVecFlat <- CorpusSourceText$new(name, dataSource, flat = TRUE)$build()$getResult()
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
    corpusVec <- CorpusSourceText$new(name, dataSource)$build()$getResult()
    corpusVecContent <- corpusVec$read()
    stopifnot(length(corpusVecContent) == 3)
    corpusVecDocuments <- corpusVec$getDocuments()
    stopifnot(length(corpusVecDocuments) == 3)
    print(corpusVec$meta())
    print(corpusVec$docMeta())

    # Build Corpus from list
    name <- "corpusList"
    dataSource <- corpusList
    corpusList <- CorpusSourceText$new(name, dataSource)$build()$getResult()
    corpusListContent <- corpusList$read()
    stopifnot(length(corpusListContent) == 3)
    corpusListDocuments <- corpusList$getDocuments()
    stopifnot(length(corpusListDocuments) == 3)
    print(corpusList$meta())
    print(corpusList$docMeta())

    CorpusSourceTextTest$logs(className = "CorpusSourceText", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusSourceTextTest$logs(className = "CorpusSourceText", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusSourceTextTest$logs(className = "CorpusSourceText", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: CorpusSourceText: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    CorpusSourceTextTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
raw <- test0()



}
className <- "CorpusSourceText"
#source('./test/unitTests/testCorpusSourceText.R')
testCorpusSourceText()
