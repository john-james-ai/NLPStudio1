testBuildData <- function() {

  init <- function() {

    # Clean up
    if (exists("sfc", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("sfc", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("oxford", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("oxford", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("en_US.blogs.txt", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.blogs.txt", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("en_US.news.txt", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.news.txt", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("en_US.twitter.txt", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.twitter.txt", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
  }

  test0 <- function() {
    test <- "test0: Corpus Builder: instantiation"
    cat(paste0("\n",test, " Commencing\n"))

    # Obtain NLPStudio object
    nlpStudio <- NLPStudio$new()$getInstance()

    # Instantiate builder
    #b <- CorpusBuilder$new() # should fail, non name success
    b <- BuildData$new(name = "sfc")

    # Check product
    p <- b$exposeObject()
    stopifnot(p$name == 'sfc')
    stopifnot(p$desc == 'sfc corpus')
    stopifnot(p$parent$getName() == 'nlpStudio')
    stopifnot(p$path == './NLPStudio/corpora/sfc')

    # Logit
    buildDataTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized corpus"))
    cat(paste0(test, " Completed: Success!\n"))

    return(b)
  }

  test1 <- function(builder) {
    test <- "test1: Corpus Builder: getData"
    cat(paste0("\n",test, " Commencing\n"))


    builder$url = 'http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
    builder$files <- c('final/en_US/en_US.blogs.txt', 'final/en_US/en_US.news.txt', 'final/en_US/en_US.twitter.txt')
    status <- builder$getData()

    # Logit
    buildDataTest$logs(className = className, methodName = "addCorpus", msg = paste("Successfully downloaded and unzipped data"))
    cat(paste0(test, " Completed: Success!\n"))

    return(status)
  }

  test2 <- function(builder) {
    test <- "test2: Corpus Builder: buildDocuments"
    cat(paste0("\n",test, " Commencing\n"))

    builder$buildDocuments()
    p <- builder$exposeObject()
    stopifnot(length(p$rawDocs) == 3)

    # Logit
    buildDataTest$logs(className = className, methodName = "addCorpus", msg = paste("Successfully built documents."))
    cat(paste0(test, " Completed: Success!\n"))

    return(status)
  }

init()
builder <- test0()
builder <- test1(builder)
builder <- test2(builder)
}
className <- "BuildData"
buildDataTest <- LogTest$new()
#source('./test/unitTests/testStudio.R')
testBuildData()
