testBuildDataRawWebZip <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("raw", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("raw", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    BuildDataRawWebZipTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: BuildDataRawWebZip"
    cat(paste0("\n",test, " Commencing\n"))

    # Parameters
    path = './test/testData/raw'
    params = list()
    params[['url']] <- 'http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
    params[['filenames']] <- c("final/en_US/en_US.blogs.txt", "final/en_US/en_US.news.txt",
                   "final/en_US/en_US.twitter.txt")

    # Validation
    #raw <- BuildDataRawWebZip$new()# should fail -success
    #raw <- BuildDataRawWebZip$new(name = 'raw') # should fail, no path
    #raw <- BuildDataRawWebZip$new(name = 'raw', path = path) # should fail, params
    #raw <- BuildDataRawWebZip$new(name = 'raw', path = path, params = newDir) # invalid URL

    # Instantiate
    raw <- BuildDataRawWebZip$new(name = 'raw', path = path, params = params) # invalid URL
    raw <- raw$execute()

    # Check files
    r <- raw$exposeObject()
    stopifnot(r$name == 'raw')
    stopifnot(r$path == path)
    stopifnot(length(r$files) == 3)

    # Logit
    BuildDataRawWebZipTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized BuildDataRawWebZip"))
    BuildDataRawWebZipTest$logs(className = className, methodName = "execute", msg = paste("Successfully sourced the data"))
    cat(paste0(test, " Completed: Success!\n"))

    return(raw)
  }

init()
raw <<- test0()
}
className <- "BuildDataRawWebZip"
testBuildDataRawWebZip()
