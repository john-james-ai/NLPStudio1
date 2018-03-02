testSourceDir <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    SourceDirTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: SourceDir: Directory"
    cat(paste0("\n",test, " Commencing\n"))

    # Init params
    name <- "fast"
    desc <- "Creating corpus from directory sources (Fast)"
    corpusSource <- "./test/testData/fast"

    # Validation
    #corpus <- SourceDir$new() # missing params
    #corpus <- SourceDir$new("foo") # missing param data source
    #corpus <- SourceDir$new(222, "corpusSource") # invalid name
    #corpus <- SourceDir$new("foo bar", corpusSource) # invalid name
    #corpus <- SourceDir$new(TRUE, corpusSource) # invalid name
    #corpus <- SourceDir$new(Entity, corpusSource) # invalid name
    #corpus <- SourceDir$new(222, corpusSource) # invalid name
    #corpus <- SourceDir$new(newsTxt, corpusSource) # invalid name
    #corpus <- SourceDir$new(start, corpusSource) # invalid name
    #corpus <- SourceDir$new("name", 22) # invalid data source
    #corpus <- SourceDir$new("name", TRUE) # invalid data source
    #corpus <- SourceDir$new(name, "./test/testData/input/*.doc")$build()$getResult() # invalid data source

    # Build Corpus from directory source
    corpus <- SourceDir$new(name, corpusSource)$build()$getResult()
    corpusContent <- corpus$read()
    stopifnot(length(corpusContent) == 3)
    docs <- corpus$getDocuments()
    stopifnot(length(docs) == 3)
    corpus$meta(key = "desc", value = desc)
    corpus$docMeta(key = "year", value = "2018")
    print(corpus$meta())
    print(corpus$docMeta())

    SourceDirTest$logs(className = "SourceDir", methodName = "initiate", msg = paste("Successfully instantiated. "))
    SourceDirTest$logs(className = "SourceDir", methodName = "build", msg = paste("Successfully instantiated. "))
    SourceDirTest$logs(className = "SourceDir", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(corpus)
  }

  test1 <- function() {
    test <- "test1: SourceDir: WildCard"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from Vector Flat
    name <- "SourceDir"
    desc <- "Creating corpus from directory sources"
    corpusSource <- "./test/testData/input/*s.txt"
    corpus <- SourceDir$new(name, corpusSource)$build()$getResult()
    corpusContent <- corpus$read()
    stopifnot(length(corpusContent) == 2)
    docs <- corpus$getDocuments()
    stopifnot(length(docs) == 2)
    corpus$meta(key = "desc", value = desc)
    corpus$docMeta(key = "year", value = "2018")
    print(corpus$meta())
    print(corpus$docMeta())

    SourceDirTest$logs(className = "SourceDir", methodName = "initiate", msg = paste("Successfully instantiated. "))
    SourceDirTest$logs(className = "SourceDir", methodName = "build", msg = paste("Successfully instantiated. "))
    SourceDirTest$logs(className = "SourceDir", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }
  
  test2 <- function() {
    test <- "test2: SourceDir: Repair"
    cat(paste0("\n",test, " Commencing\n"))
    
    # Init params
    name <- "corpus"
    desc <- "Creating corpus from directory sources"
    corpusSource <- "./test/testData/hc"
    
    
    # Build corpus without repair
    corpus1 <<- SourceDir$new(name, corpusSource, repair = FALSE)$build()$getResult()
    corpus2 <<- SourceDir$new(name, corpusSource)$build()$getResult()
    docs1 <- corpus1$getDocuments()
    docs2 <- corpus2$getDocuments()
    lapply(seq_along(docs1), function(d) {
      c1 <- docs1[[d]]$text
      c2 <- docs2[[d]]$text
      print(paste("Document", docs1[[d]]$getName(), "has length", length(c1)))
      print(paste("Document", docs2[[d]]$getName(), "has length", length(c2)))
      
    })
    
    lapply(seq_along(docs1), function(d) {
      c1 <- docs1[[d]]$text
      c2 <- docs2[[d]]$text
      print(head(c1, 1))
      print(head(c2, 1))
      
    })
    
    SourceDirTest$logs(className = "SourceDir", methodName = "repair", msg = paste("Successfully repaired corpus "))
    cat(paste0(test, " Completed: Success!\n"))
    
  }
  


  testn <- function() {
    test <- "testn: SourceDir: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    SourceDirTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
corpus0 <<- test0()
corpus1 <<- test1()
test2()


}
className <- "SourceDir"
#source('./test/unitTests/testSourceDir.R')
testSourceDir()
