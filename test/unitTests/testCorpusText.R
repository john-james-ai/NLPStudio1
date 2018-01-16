testCorpusImportText <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusImportTextTest <<- LogTest$new()
    files <- list.files(path = "./test/testData/input", full.names = TRUE)
    corpus <<- lapply(files, function(f) {
      readLines(f)
    })
    bigBlogs <<- readLines("./test/testData/hc/en_US.blogs.txt")
  }

  test0 <- function() {
    test <- "test0: CorpusImportText"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from Vector Flat
    name <- "corpusVecFlat"
    desc <- "Creating corpus from flat vector"
    docDesc <- c("Blogs for Slogs", "News of the World", "Tweets for Twits")
    dataSource <- corpus[[2]]
    corpusVecFlat <- CorpusImportText$new(name, dataSource, flat = TRUE)$build()$getResult()
    corpusVecFlatContent <- corpusVecFlat$read()
    stopifnot(length(corpusVecFlatContent[[1]]) == 2000)
    corpusVecFlatDocuments <- corpusVecFlat$getDocuments()
    stopifnot(length(corpusVecFlatDocuments) == 1)

    # Update content
    corpusVecFlatDocuments[[1]]$content <- bigBlogs
    corpusVecFlatContent <- corpusVecFlat$read()
    stopifnot(length(corpusVecFlatContent[[1]]) > 2000)
    stopifnot(length(corpusVecFlatDocuments[[1]]$content) > 2000)

    # Update metadata
    corpusVecFlat$meta(key = "name", value = name)
    corpusVecFlat$meta(key = "desc", value = desc)
    corpusVecFlat$docMeta(key = "name", value = paste0(name, "-document"))
    corpusVecFlat$docMeta(key = "desc", docDesc[[2]])
    print(corpusVecFlat$meta())
    print(corpusVecFlat$docMeta())

    # Change Content
    corpusVecFlatDocuments[[1]]$content <- bigBlogs

    # Build Corpus from vector non-flat
    name <- "corpusVec"
    docNames <- c("blogs", "news", "twitter")
    desc <- "Creating corpus from vector"
    qc <- quanteda::corpus(readtext::readtext("./test/testData/input/*.txt"))
    dataSource <- qc$documents$texts
    corpusVec <- CorpusImportText$new(name, dataSource)$build()$getResult()
    corpusVecContent <- corpusVec$read()
    stopifnot(length(corpusVecContent) == 3)
    corpusVecDocuments <- corpusVec$getDocuments()
    stopifnot(length(corpusVecDocuments) == 3)

    # Update content
    corpusVecDocuments[[1]]$content <- bigBlogs
    corpusVecContent <- corpusVec$read()
    stopifnot(length(corpusVecContent[[1]]) > 2000)
    stopifnot(length(corpusVecDocuments[[1]]$content) > 2000)

    # Update metadata
    corpusVec$meta(key = "name", value = name)
    corpusVec$meta(key = "desc", value = desc)
    corpusVec$docMeta(key = "name", value = docNames)
    corpusVec$docMeta(key = "desc", docDesc)
    print(corpusVec$meta())
    print(corpusVec$docMeta())

    # Build Corpus from list
    name <- "corpusList"
    desc <- "Creating corpus from list"
    dataSource <- corpus
    corpusList <- CorpusImportText$new(name, dataSource)$build()$getResult()
    corpusListContent <- corpusList$read()
    stopifnot(length(corpusListContent) == 3)
    stopifnot(length(corpusListContent[[1]]) == 2000)
    stopifnot(length(corpusListContent[[2]]) == 2000)
    stopifnot(length(corpusListContent[[3]]) == 2000)
    corpusListDocuments <- corpusList$getDocuments()
    stopifnot(length(corpusListDocuments) == 3)

    # Update content
    corpusListDocuments[[1]]$content <- bigBlogs
    corpusListContent <- corpusList$read()
    stopifnot(length(corpusListContent[[1]]) > 2000)
    stopifnot(length(corpusListDocuments[[1]]$content) > 2000)

    # Update metadata
    corpusList$meta(key = "name", value = name)
    corpusList$meta(key = "desc", value = desc)
    corpusList$docMeta(key = "name", value = docNames)
    corpusList$docMeta(key = "desc", docDesc)
    corpusVecFlatDocuments[[1]]$content <- bigBlogs
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
