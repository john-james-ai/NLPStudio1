testSourceDir <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    SourceDirTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: Create corpus"
    cat(paste0("\n",test, " Commencing\n"))

    # Init params
    name <- "fast"
    desc <- "Creating corpus from directory sources (Fast)"
    corpusSource <- "./test/testData/fast"

    # Build Corpus from directory source
    corpus <- SourceDir$new(name, corpusSource)$build()$getResult()
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

    SourceDirTest$logs(className = "SourceDir", methodName = "initiate", msg = paste("Successfully instantiated. "))
    SourceDirTest$logs(className = "SourceDir", methodName = "build", msg = paste("Successfully instantiated. "))
    SourceDirTest$logs(className = "SourceDir", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(corpus)
  }

  

  test1 <- function(corpus) {
    test <- "test1: Create tm corpus"
    cat(paste0("\n",test, " Commencing\n"))
    
    tmCorpus <- AdaptorTM$new(x = corpus, format = "tm")$adapt()
    print(NLP::meta(tmCorpus, type = "corpus"))
    print(NLP::meta(tmCorpus, type = "local"))

    SourceDirTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(tmCorpus)
  }
  
  test2 <- function(corpus) {
    test <- "test2: Create NLPStudio Corpus and compare"
    cat(paste0("\n",test, " Commencing\n"))
    
    corpusNew <- AdaptorTM$new(x = corpus, format = "tm")$adapt()
    # for (i in length(corpus$meta())) {
    #   stopifnot(all.equal(corpus$meta()[[i]], corpusNew$metadata[[i]]))
    # }
    print("Old Corpus Metadata")
    print(corpus$meta())
    print("New Corpus Metadata")
    print(lapply(corpusNew, function(x) {x$meta}))
    print(as.data.frame(corpusNew$metadata))
    print("Old Corpus Document Metadata")
    print(corpus$docMeta())
    print("New Corpus Document Metadata")
    print(lapply(corpusNew, function(x) {x$meta}))
    
    SourceDirTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))
    
    return(tmCorpus)
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
corpus <<- test0()
tmCorpus <<- test1(corpus)
corpus <<- test2(corpus)


}
className <- "SourceDir"
#source('./test/unitTests/testSourceDir.R')
testSourceDir()
