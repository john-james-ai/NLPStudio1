testDocumentText <- function() {

  init <- function() {
    if (exists("en_US.twitter.txt", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.twitter.txt", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("en_US.news.txt", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.news.txt", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("en_US.blogs.txt", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.blogs.txt", ls(envir = .GlobalEnv))], envir = .GlobalEnv)

  }

  test0 <- function() {
    test <- "test0: DocumentText: Instantiate and Read Document"
    cat(paste0("\n",test, " Commencing\n"))

    # Instantiate
    blogs <- DocumentText$new('./test/testData/hc/en_US.blogs.txt', desc = 'Blogs Register')
    d <- blogs$exposeObject()
    stopifnot(d$className == 'DocumentText')
    stopifnot(d$name == 'en_US.blogs.txt')
    stopifnot(d$fileName == 'en_US.blogs.txt')
    stopifnot(d$desc == 'Blogs Register')
    stopifnot(d$path == './test/testData/hc/en_US.blogs.txt')
    stopifnot(length(d$content) > 1000)

    # Logit
    DocumentTextTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized corpus"))
    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test1 <- function(blogs) {
    test <- "test1: DocumentText: Move document"
    cat(paste0("\n",test, " Commencing\n"))

    # Logit
    DocumentTextTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized corpus"))
    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

init()
blogs <- test0()
blogs <- test1(blogs)
}
className <- "DocumentText"
DocumentTextTest <- LogTest$new()
#source('./test/unitTests/testCorpusBuilder.R')
testDocumentText()
