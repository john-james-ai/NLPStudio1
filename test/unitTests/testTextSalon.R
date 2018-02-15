testTextStudio <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    TextStudioTest <<- LogTest$new()
    bf <- "./test/testData/fast/en_US.blogs.txt"
    nf <- "./test/testData/fast/en_US.news.txt"
    tf <- "./test/testData/fast/en_US.twitter.txt"
    blogsTxt <<- readLines(bf)
    newsTxt <<- readLines(nf)
    twitsTxt <<- readLines(tf)
  }

  test0 <- function() {
    test <- "test0: TextStudio"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from directory source
    name <- "Corpus"
    desc <- "Creating corpus from directory sources"
    dataSource <- "./test/testData/fast"
    tokens <- c("You", "you", "plant", "Been", "been", "home")
    replace <- c("BITCH", "BOAT", "DANCE", "LOVE", "PARTY", "HOPE")
    

    # Import corpus and get contents
    corpus <- CorpusSourceDir$new(name, dataSource)$build()$getResult()
    docs <- corpus$getDocuments()
    docs1 <- lapply(docs, function(d) {
      d$content
    })

    # Preprocess
    ts <- TextStudio$new(corpus)
    cmd <- TokenizeCmd$new(whatA = "sentence")
    ts <- ts$addCommand(cmd)
    corpus2 <- ts$execute()$getResult()

    # Get Documents
    docs <- corpus2$getDocuments()
    docs2 <- lapply(docs, function(d) {
      d$content
    })

    for (i in 1:length(docs)) {
      cat(paste("\nDocument", i, "\n"))
      print(head(docs1[[i]], 2))
      print(head(docs2[[i]], 2))
    }

    TextStudioTest$logs(className = "TextStudio", methodName = "initiate", msg = paste("Successfully instantiated. "))
    TextStudioTest$logs(className = "TextStudio", methodName = "execute", msg = paste("Processing successfully executed."))
    TextStudioTest$logs(className = "TextStudio", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(corpus2)
  }


  testn <- function() {
    test <- "testn: TextStudio: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    TextStudioTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }

  wrapup <- function() {
    bf <- "./test/testData/fast/en_US.blogs.txt"
    nf <- "./test/testData/fast/en_US.news.txt"
    tf <- "./test/testData/fast/en_US.twitter.txt"
    writeLines(blogsTxt, bf)
    writeLines(newsTxt, nf)
    writeLines(twitsTxt, tf)
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
corpus2 <<- test0()


}
className <- "TextStudio"
#source('./test/unitTests/testTextStudio.R')
testTextStudio()
