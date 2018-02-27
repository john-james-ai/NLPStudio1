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
    corpus$meta(key = "desc", value = desc)
    corpus$docMeta(key = "year", value = "2018")
    print(corpus$meta())
    print(corpus$docMeta())

    CorpusSourceDirTest$logs(className = "CorpusSourceDir", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusSourceDirTest$logs(className = "CorpusSourceDir", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusSourceDirTest$logs(className = "CorpusSourceDir", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(corpus)
  }

  test1 <- function() {
    test <- "test1: CorpusSourceDir: WildCard"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from Vector Flat
    name <- "CorpusSourceDir"
    desc <- "Creating corpus from directory sources"
    dataSource <- "./test/testData/input/*s.txt"
    corpus <- CorpusSourceDir$new(name, dataSource)$build()$getResult()
    corpusContent <- corpus$read()
    stopifnot(length(corpusContent) == 2)
    docs <- corpus$getDocuments()
    stopifnot(length(docs) == 2)
    corpus$meta(key = "desc", value = desc)
    corpus$docMeta(key = "year", value = "2018")
    print(corpus$meta())
    print(corpus$docMeta())

    CorpusSourceDirTest$logs(className = "CorpusSourceDir", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusSourceDirTest$logs(className = "CorpusSourceDir", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusSourceDirTest$logs(className = "CorpusSourceDir", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }
  
  test2 <- function() {
    test <- "test2: CorpusSourceDir: Repair"
    cat(paste0("\n",test, " Commencing\n"))
    
    # Init params
    name <- "corpus"
    desc <- "Creating corpus from directory sources"
    dataSource <- "./test/testData/hc"
    
    
    # Build corpus without repair
    corpus1 <<- CorpusSourceDir$new(name, dataSource, repair = FALSE)$build()$getResult()
    corpus2 <<- CorpusSourceDir$new(name, dataSource)$build()$getResult()
    docs1 <- corpus1$getDocuments()
    docs2 <- corpus2$getDocuments()
    lapply(seq_along(docs1), function(d) {
      c1 <- docs1[[d]]$content
      c2 <- docs2[[d]]$content
      print(paste("Document", docs1[[d]]$getName(), "has length", length(c1)))
      print(paste("Document", docs2[[d]]$getName(), "has length", length(c2)))
      
    })
    
    lapply(seq_along(docs1), function(d) {
      c1 <- docs1[[d]]$content
      c2 <- docs2[[d]]$content
      print(head(c1, 1))
      print(head(c2, 1))
      
    })
    
    CorpusSourceDirTest$logs(className = "CorpusSourceDir", methodName = "repair", msg = paste("Successfully repaired corpus "))
    cat(paste0(test, " Completed: Success!\n"))
    
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
