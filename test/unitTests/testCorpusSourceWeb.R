testCorpusSourceWeb <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
  }

  test0 <- function() {
    test <- "test0: CorpusSourceWeb"
    cat(paste0("\n",test, " Commencing\n"))

    # Parameters
    url <- 'http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
    fileNames <- c("final/en_US/en_US.blogs.txt", "final/en_US/en_US.news.txt",
                   "final/en_US/en_US.twitter.txt")

    # Validation
    #csw <- CorpusSourceWeb$new()# should fail -success
    #csw <- CorpusSourceWeb$new(url = 'url', fileNames = fileNames) # should fail

    # Instantiate
    csw <- CorpusSourceWeb$new(url = url, fileNames = fileNames)
    csw$sourceData()
    d <- csw$exposeObject()
    stopifnot(length(d$files) == 3)

    # Logit
    DocumentTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized CorpusSourceWeb"))
    DocumentTest$logs(className = className, methodName = "sourceData", msg = paste("Successfully sourced the data"))
    cat(paste0(test, " Completed: Success!\n"))

    return(d)
  }

init()
csw <<- test0()
}
className <- "CorpusSourceWeb"
#source('./test/unitTests/testCorpusBuilder.R')
testCorpusSourceWeb()
