testSourceDataWeb <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("rawData", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("rawData", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
  }

  test0 <- function() {
    test <- "test0: SourceDataWeb"
    cat(paste0("\n",test, " Commencing\n"))

    # Parameters
    url <- 'http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
    fileNames <- c("final/en_US/en_US.blogs.txt", "final/en_US/en_US.news.txt",
                   "final/en_US/en_US.twitter.txt")

    # Validation
    # csw <- SourceDataWeb$new()# should fail -success
    # csw <- SourceDataWeb$new(url = 'www.data.com') # should fail, bad url
    # csw <- SourceDataWeb$new(url = url) # should fail, no name

    # Instantiate
    csw <- SourceDataWeb$new(name = 'rawData', url = url, desc = 'Raw Corpus')
    csw$sourceData()

    # Check files
    s <- csw$exposeObject()


    stopifnot(length(d$files) == 3)

    # Logit
    DocumentTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized SourceDataWeb"))
    DocumentTest$logs(className = className, methodName = "sourceData", msg = paste("Successfully sourced the data"))
    cat(paste0(test, " Completed: Success!\n"))

    return(d)
  }

init()
csw <<- test0()
}
className <- "SourceDataWeb"
#source('./test/unitTests/testCorpusBuilder.R')
testSourceDataWeb()
