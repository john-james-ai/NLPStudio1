testCorpusSourceDir <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusSourceDirTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: CorpusSourceDir: Directory"
    cat(paste0("\n",test, " Commencing\n"))

    # Init params
    name <- "fast"
    desc <- "Creating corpus from directory sources (Fast)"
    dataSource <- "./test/testData/fast"

    # Validation
    #corpus <- CorpusSourceDir$new() # missing params
    #corpus <- CorpusSourceDir$new("foo") # missing param data source
    #corpus <- CorpusSourceDir$new(222, "dataSource") # invalid name
    #corpus <- CorpusSourceDir$new("foo bar", "dataSource") # invalid name
    #corpus <- CorpusSourceDir$new(TRUE, "dataSource") # invalid name
    #corpus <- CorpusSourceDir$new(Entity, "dataSource") # invalid name
    #corpus <- CorpusSourceDir$new(222, "dataSource") # invalid name
    #corpus <- CorpusSourceDir$new(newsTxt, "dataSource") # invalid name
    #corpus <- CorpusSourceDir$new(start, "dataSource") # invalid name
    #corpus <- CorpusSourceDir$new(name, 22) # invalid data source
    #corpus <- CorpusSourceDir$new(name, TRUE) # invalid data source
    #corpus <- CorpusSourceDir$new(name, "./test/testData/input/*.doc")$build()$getResult() # invalid data source

    # Build Corpus from directory source
    corpus <- CorpusSourceDir$new(name, dataSource)$build()$getResult()
    corpusContent <- corpus$read()
    stopifnot(length(corpusContent) == 3)
    docs <- corpus$getDocuments()
    stopifnot(length(docs) == 3)
    corpus$meta(key = "Description", value = desc)
    corpus$docMeta(key = "Year", value = "2018")
    corpus$docMeta(key = "Genre", value = c("Blogs", "News", "Tweets"))
    print(corpus$meta())
    print(corpus$docMeta())

    CorpusSourceDirTest$logs(className = "CorpusSourceDir", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusSourceDirTest$logs(className = "CorpusSourceDir", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusSourceDirTest$logs(className = "CorpusSourceDir", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(corpus)
  }

  

  testn <- function() {
    test <- "testn: CorpusSourceDir: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    CorpusSourceDirTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
corpus0 <<- test0()
corpus1 <<- test1()
test2()


}
className <- "CorpusSourceDir"
#source('./test/unitTests/testCorpusSourceDir.R')
testCorpusSourceDir()
