testSourceVector <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    SourceVectorTest <<- LogTest$new()
    files <- list.files(path = "./test/testData/input", full.names = TRUE)
    corpus <<- lapply(files, function(f) {
      readLines(f)
    })


  }

  test0 <- function() {
    test <- "test0: SourceVector"
    cat(paste0("\n",test, " Commencing\n"))

    name <- "corpusVecFlat"

    # Validation
    #corpus <- SourceVector$new() # missing params
    #corpus <- SourceVector$new("foo") # missing param data source
    corpus <- SourceVector$new("foo", "corpusSource") # invalid name

    # Build Corpus from Vector Flat
    news <- readLines("./test/testData/input/en_US.news.txt")
    corpusSource <- news
    corpusVecFlat <- SourceVector$new(name, corpusSource, flat = TRUE)$build()$getResult()
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
    corpusSource <- qc$documents$texts
    corpusVec <- SourceVector$new(name, corpusSource)$build()$getResult()
    corpusVecContent <- corpusVec$read()
    stopifnot(length(corpusVecContent) == 3)
    corpusVecDocuments <- corpusVec$getDocuments()
    stopifnot(length(corpusVecDocuments) == 3)
    print(corpusVec$meta())
    print(corpusVec$docMeta())

    # Build Corpus from list
    name <- "corpusList"
    corpusSource <- corpusList
    corpusList <- SourceVector$new(name, corpusSource)$build()$getResult()
    corpusListContent <- corpusList$read()
    stopifnot(length(corpusListContent) == 3)
    corpusListDocuments <- corpusList$getDocuments()
    stopifnot(length(corpusListDocuments) == 3)
    print(corpusList$meta())
    print(corpusList$docMeta())

    SourceVectorTest$logs(className = "SourceVector", methodName = "initiate", msg = paste("Successfully instantiated. "))
    SourceVectorTest$logs(className = "SourceVector", methodName = "build", msg = paste("Successfully instantiated. "))
    SourceVectorTest$logs(className = "SourceVector", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: SourceVector: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    SourceVectorTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
raw <- test0()



}
className <- "SourceVector"
#source('./test/unitTests/testSourceVector.R')
testSourceVector()
