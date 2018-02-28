testCorpusSourceDir <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusSourceDirTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: Create corpus"
    cat(paste0("\n",test, " Commencing\n"))

    # Init params
    name <- "fast"
    desc <- "Creating corpus from directory sources (Fast)"
    dataSource <- "./test/testData/fast"

    # Build Corpus from directory source
    corpus <- CorpusSourceDir$new(name, dataSource)$build()$getResult()
    corpusContent <- corpus$read()
    stopifnot(length(corpusContent) == 3)
    docs <- corpus$getDocuments()
    stopifnot(length(docs) == 3)
    
    # Add corpus metadata
    corpus$meta(key = "Description", value = desc)
    corpus$meta(key = "Author", value = "HC Corpus")
    print(corpus$meta())
    
    # Add document metadata
    corpus$docMeta(key = "Year", value = "2018")
    corpus$docMeta(key = "Genre", value = c("Blogs", "News", "Tweets"))
    print(corpus$docMeta())

    CorpusSourceDirTest$logs(className = "CorpusSourceDir", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusSourceDirTest$logs(className = "CorpusSourceDir", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusSourceDirTest$logs(className = "CorpusSourceDir", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(corpus)
  }

  

  test1 <- function(corpus) {
    test <- "test1: Create quanteda corpus"
    cat(paste0("\n",test, " Commencing\n"))
    
    qCorpus <- AdaptorQ$new(x = corpus, format = "q")$adapt()
    print(quanteda::metacorpus(qCorpus))
    print(quanteda::metadoc(qCorpus))

    CorpusSourceDirTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(qCorpus)
  }
  
  test2 <- function(corpus) {
    test <- "test2: Create NLPStudio Corpus and compare"
    cat(paste0("\n",test, " Commencing\n"))
    
    corpusNew <- AdaptorQ$new(x = corpus, format = "q")$adapt()
    for (i in length(corpus$meta())) {
      stopifnot(all.equal(corpus$meta()[[i]], corpusNew$metadata[[i]]))
    }
    print("Old Corpus Metadata")
    print(corpus$meta())
    print("New Corpus Metadata")
    print(as.data.frame(corpusNew$metadata))
    print("Old Corpus Document Metadata")
    print(corpus$docMeta())
    print("New Corpus Document Metaata")
    print(corpusNew$documents[,c(2:ncol(corpusNew$documents))])
    
    CorpusSourceDirTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))
    
    return(qCorpus)
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
corpus <<- test0()
qCorpus <<- test1(corpus)
corpus <<- test2(corpus)


}
className <- "CorpusSourceDir"
#source('./test/unitTests/testCorpusSourceDir.R')
testCorpusSourceDir()
