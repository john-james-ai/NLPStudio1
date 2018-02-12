testTextSalon <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    TextSalonTest <<- LogTest$new()
    bf <- "./test/testData/fast/en_US.blogs.txt"
    nf <- "./test/testData/fast/en_US.news.txt"
    tf <- "./test/testData/fast/en_US.twitter.txt"
    blogsTxt <<- readLines(bf)
    newsTxt <<- readLines(nf)
    twitsTxt <<- readLines(tf)
  }

  test0 <- function() {
    test <- "test0: TextSalon"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from directory source
    name <- "Corpus"
    desc <- "Creating corpus from directory sources"
    dataSource <- "./test/testData/fast"
    terms <- c("You", "you", "plant", "Been", "been", "home")
    replace <- c("BITCH")

    # Import corpus and get contents
    corpus <- CorpusImportDir$new(name, dataSource)$build()$getResult()
    docs <- corpus$getDocuments()
    docs1 <- lapply(docs, function(d) {
      d$content
    })

    # Preprocess
    ts <- TextSalon$new(corpus)
    cmd <- CmdStripText$new()
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

    TextSalonTest$logs(className = "TextSalon", methodName = "initiate", msg = paste("Successfully instantiated. "))
    TextSalonTest$logs(className = "TextSalon", methodName = "execute", msg = paste("Processing successfully executed."))
    TextSalonTest$logs(className = "TextSalon", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(corpus2)
  }


  testn <- function() {
    test <- "testn: TextSalon: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    TextSalonTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
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
className <- "TextSalon"
#source('./test/unitTests/testTextSalon.R')
testTextSalon()
