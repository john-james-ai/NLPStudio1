testTokenize <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    TokenizeTest <<- LogTest$new()
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

    TokenizeTest$logs(className = "CorpusSourceDir", methodName = "initiate", msg = paste("Successfully instantiated. "))
    TokenizeTest$logs(className = "CorpusSourceDir", methodName = "build", msg = paste("Successfully instantiated. "))
    TokenizeTest$logs(className = "CorpusSourceDir", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(corpus)
  }
  
  test1 <- function(c1) {
    test <- "test1: Tokenize"
    cat(paste0("\n",test, " Commencing\n"))
    
    
    c2 <- Tokenize$new(x = c1, what = "sentence")$execute()
    d2s <- c2$getDNA(id = "Tokenize-sentence")
    
    # Compare corpus level meta data
    print(c1$meta())
    print(c2$meta())
    
    # Get Documents
    c1d <- c1$getDocuments()
    c2d <- c2$getDocuments()
    for (i in 1:length(c1d)) {
      cat("\n\nMetadata\n")
      print(c1d[[i]]$meta())
      print(c2d[[i]]$meta())
      print(paste("c1d length is:", length(c1d[[i]]$text),
                  "c2d length is:", length(c2d[[i]]$text)))
    }
    return(c2)
  }

init()
#c1 <<- test0()
c2 <<- test1(c1)


}
className <- "Tokenize"
#source('./test/unitTests/testTokenize.R')
testTokenize()
