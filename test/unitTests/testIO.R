testIO <- function() {

  init <- function() {
    if (exists("en_US.twitter.txt", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.twitter.txt", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("en_US.news.txt", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.news.txt", ls(envir = .GlobalEnv))], envir = .GlobalEnv)

  }

  test0 <- function() {
    test <- "test0: IOText: Read Document"
    cat(paste0("\n",test, " Commencing\n"))

    # Create document
    news <- Document$new("./NLPStudios/corpora/sfc/data/raw/en_US.news.txt", desc = 'News')
    d <- news$exposeObject()
    stopifnot(d$name == "en_US.news.txt")

    # Read document
    io <- IOText$new()
    status <- io$read(news)
    stopifnot(status[['code']] == TRUE)
    stopifnot(length(status[['data']]) > 1000)
    content <- status[['data']]
    head(content)

    ## Write Document
    io$write(news)

    # Logit
    IOTestLog$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized corpus"))
    cat(paste0(test, " Completed: Success!\n"))

    return(news)
  }

  test1 <- function(news) {
    test <- "test1: IOBin: Read Document"
    cat(paste0("\n",test, " Commencing\n"))

    # Read document
    io <- IOBin$new()
    status <- io$read(news)
    stopifnot(status[['code']] == TRUE)
    stopifnot(length(status[['data']]) > 1000)
    content <- status[['data']]
    head(content)

    ## Write Document
    news$setContent(content[1:100])
    io$write(news)

    # Logit
    IOTestLog$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized corpus"))
    cat(paste0(test, " Completed: Success!\n"))

    return(news)
  }


init()
news <- test0()
news <- test1(news)
}
className <- "CorpusBuilder"
IOTestLog <- LogTest$new()
#source('./test/unitTests/testCorpusBuilder.R')
testIO()
