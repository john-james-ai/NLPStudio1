testCorpusText <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusTextTest <<- LogTest$new()
    files <- list.files(path = "./test/testData/input", full.names = TRUE)
    corpus <<- lapply(files, function(f) {
      readLines(f)
    })


  }

  test0 <- function() {
    test <- "test0: CorpusText"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from Vector Flat
    name <- "corpusVecFlat"
    path <- "./test/testData/corpusBuilderRAwTextSource/corpusVecFlat"
    news <- readLines("./test/testData/input/en_US.news.txt")
    dataSource <- news
    corpusVecFlat <- CorpusText$new(name, path, dataSource, flat = TRUE)$build()$getResult()
    corpusVecFlatContent <- corpusVecFlat$read()
    stopifnot(length(corpusVecFlatContent$corpusVec) == 2000)
    stopifnot(corpusVecFlatContent$corpusVec == news)
    corpusVecFlatDocuments <- corpusVecFlat$getDocuments()
    stopifnot(length(corpusVecFlatDocuments) == 1)
    print(corpusVecFlat$meta())
    print(corpusVecFlat$docMeta())

    # Build Corpus from vector non-flat
    name <- "corpusVec"
    path <- "./test/testData/corpusBuilderRAwTextSource/corpusVec"
    qc <- quanteda::corpus(readtext::readtext("./test/testData/input/*.txt"))
    dataSource <- qc$documents$texts
    corpusVec <- CorpusText$new(name, path, dataSource)$build()$getResult()
    corpusVecContent <- corpusVec$read()
    stopifnot(length(corpusVecContent) == 3)
    corpusVecDocuments <- corpusVec$getDocuments()
    stopifnot(length(corpusVecDocuments) == 3)
    print(corpusVec$meta())
    print(corpusVec$docMeta())

    # Build Corpus from list
    name <- "corpusList"
    path <- "./test/testData/corpusBuilderRAwTextSource/corpusList"
    dataSource <- corpus
    corpusList <- CorpusText$new(name, path, dataSource)$build()$getResult()
    corpusListContent <- corpusList$read()
    stopifnot(length(corpusListContent) == 3)
    corpusListDocuments <- corpusList$getDocuments()
    stopifnot(length(corpusListDocuments) == 3)
    print(corpusList$meta())
    print(corpusList$docMeta())

    CorpusTextTest$logs(className = "CorpusText", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusTextTest$logs(className = "CorpusText", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusTextTest$logs(className = "CorpusText", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: CorpusText: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    CorpusTextTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
raw <- test0()



}
className <- "CorpusText"
#source('./test/unitTests/testCorpusText.R')
testCorpusText()
