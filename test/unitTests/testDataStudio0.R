testDataStudio0 <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    DataStudio0Test <<- LogTest$new()
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

    DataStudio0Test$logs(className = "SourceDir", methodName = "initiate", msg = paste("Successfully instantiated. "))
    DataStudio0Test$logs(className = "SourceDir", methodName = "build", msg = paste("Successfully instantiated. "))
    DataStudio0Test$logs(className = "SourceDir", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(corpus)
  }
  
  test1 <- function() {
    test <- "test1: Clone Corpus"
    cat(paste0("\n",test, " Commencing\n"))
    
    private$..x <- corpus
    
  }

init()
corpus <<- test0()
dataObject <<- test1(corpus)


}
className <- "SourceDir"
#source('./test/unitTests/testDataStudio0.R')
testDataStudio0()
