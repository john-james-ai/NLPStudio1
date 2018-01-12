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
    desc <- "Creating corpus from flat vector"
    dataSource <- corpus[[2]]
    corpusVecFlat <- CorpusText$new(name, dataSource, flat = TRUE)$build()$getResult()
    corpusVecFlatContent <- corpusVecFlat$getContent()
    stopifnot(length(corpusVecFlatContent[[1]]) == 2000)
    corpusVecFlatDocuments <- corpusVecFlat$getDocuments()
    stopifnot(length(corpusVecFlatDocuments) == 1)
    corpusVecFlat$meta(key = "name", value = name)
    corpusVecFlat$meta(key = "desc", value = desc)
    corpusVecFlat$docMeta(key = "name", value = paste0(name, "document", sep = "."))
    print(corpusVecFlat$meta())
    print(corpusVecFlat$docMeta())

    # Build Corpus from vector non-flat
    name <- "corpusVec"
    docNames <- c("blogs", "news", "twitter")
    desc <- "Creating corpus from vector"
    qc <- quanteda::corpus(readtext::readtext("./test/testData/input/*.txt"))
    dataSource <- qc$documents$texts
    corpusVec <- CorpusText$new(name, dataSource)$build()$getResult()
    corpusVecContent <- corpusVec$getContent()
    stopifnot(length(corpusVecContent) == 3)
    corpusVecDocuments <- corpusVec$getDocuments()
    stopifnot(length(corpusVecDocuments) == 3)
    corpusVec$meta(key = "name", value = name)
    corpusVec$meta(key = "desc", value = desc)
    corpusVec$docMeta(key = "name", value = docNames)
    print(corpusVec$meta())
    print(corpusVec$docMeta())

    # Build Corpus from list
    name <- "corpusList"
    desc <- "Creating corpus from list"
    dataSource <- corpus
    corpusList <- CorpusText$new(name, dataSource)$build()$getResult()
    corpusListContent <- corpusList$getContent()
    stopifnot(length(corpusListContent) == 3)
    stopifnot(length(corpusListContent[[1]]) == 2000)
    stopifnot(length(corpusListContent[[2]]) == 2000)
    stopifnot(length(corpusListContent[[3]]) == 2000)
    corpusListDocuments <- corpusList$getDocuments()
    stopifnot(length(corpusListDocuments) == 3)
    corpusList$meta(key = "name", value = name)
    corpusList$meta(key = "desc", value = desc)
    corpusList$docMeta(key = "name", value = docNames)
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
