testCorpusBuilder <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusBuilderTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: CorpusBuilderRaw"
    cat(paste0("\n",test, " Commencing\n"))

    # Test Directory
    dataSource <- "./test/testData/input"
    docNames = c("en_US.blogs", "en_US.news", "en_US.twitter")
    textDir <- DataSourceText$new(dataSource = dataSource)$getSource()
    stopifnot(length(textDir) == 3)
    stopifnot(length(textDir[[1]]) == 2000)
    stopifnot(length(textDir[[2]]) == 2000)
    stopifnot(length(textDir[[3]]) == 2000)
    stopifnot(names(textDir) == docNames)

    # Build Corpus
    name <- "corpusDir"
    path <- "./test/testData/corpusDir"
    corpusDir <- CorpusBuilderRaw$new(name, path, dataSource = textDir)$build()$getResult()
    corpusDirContent <- corpusDir$getContent()
    corpusDirDocumnts <- corpusDir$getDocuments()
    print(corpusDir$meta())
    print(corpusDir$docMeta())

    # Test filename
    dataSource <- "./test/testData/input/en_US.blogs.txt"
    textFile <- DataSourceText$new(dataSource = dataSource)$getSource()
    stopifnot(length(textFile) == 2000)
    stopifnot(names(textFile) == "en_US.blogs")

    # Build Corpus
    name <- "corpusFile"
    path <- "./test/testData/corpusFile"
    corpusFile <- CorpusBuilderRaw$new(name, path, dataSource = textFile)$build()$getResult()
    corpusFileContent <- corpusFile$getContent()
    corpusFileDocumnts <- corpusFile$getDocuments()
    print(corpusFile$meta())
    print(corpusFile$docMeta())

    # Test List
    dataSource <- content
    textList <- DataSourceText$new(dataSource = dataSource)$getSource()
    stopifnot(length(textList) == 3)
    stopifnot(length(textList[[1]]) == 2000)
    stopifnot(length(textList[[2]]) == 2000)
    stopifnot(length(textList[[3]]) == 2000)
    stopifnot(names(textList) == docNames)

    # Build Corpus
    name <- "corpusList"
    path <- "./test/testData/corpusList"
    corpusList <- CorpusBuilderRaw$new(name, path, dataSource = textList)$build()$getResult()
    corpusListContent <- corpusList$getContent()
    corpusListDocumnts <- corpusList$getDocuments()
    print(corpusList$meta())
    print(corpusList$docMeta())

    # Test List (Unnamed)
    docNames = c("Document1", "Document2", "Document3")
    dataSource <- content2
    textUnnamedList <- DataSourceText$new(dataSource = dataSource)$getSource()
    stopifnot(length(textUnnamedList) == 3)
    stopifnot(length(textUnnamedList[[1]]) == 2000)
    stopifnot(length(textUnnamedList[[2]]) == 2000)
    stopifnot(length(textUnnamedList[[3]]) == 2000)
    stopifnot(names(textUnnamedList) == docNames)

    # Build Corpus
    name <- "corpusUnnamedList"
    path <- "./test/testData/corpusUnnamedList"
    corpusUnnamedList <- CorpusBuilderRaw$new(name, path, dataSource = textUnnamedList)$build()$getResult()
    corpusUnnamedListContent <- corpusUnnamedList$getContent()
    corpusUnnamedListDocumnts <- corpusUnnamedList$getDocuments()
    print(corpusUnnamedList$meta())
    print(corpusUnnamedList$docMeta())

    # Test vector
    dataSource <- news
    textVec <- DataSourceText$new(dataSource = dataSource)$getSource()
    stopifnot(length(textVec) == 2000)
    stopifnot(names(textVec) == "en_US.news")

    # Build Corpus
    name <- "corpusVec"
    path <- "./test/testData/corpusVec"
    corpusVec <- CorpusBuilderRaw$new(name, path, dataSource = textVec)$build()$getResult()
    corpusVecContent <- corpusVec$getContent()
    corpusVecDocumnts <- corpusVec$getDocuments()
    print(corpusVec$meta())
    print(corpusVec$docMeta())

    CorpusBuilderTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: CorpusBuilder: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    CorpusBuilderTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
raw <- test0()



}
className <- "CorpusBuilder"
#source('./test/unitTests/testCorpusBuilder.R')
testCorpusBuilder()
