testCorpusBuilder <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("dataSource", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("dataSource", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("builder", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("builder", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("director", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("director", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("dataSource", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("dataSource", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("data", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("data", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusBuilderTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: Create Data Source"
    cat(paste0("\n",test, " Commencing\n"))

    path <- './test/testData/input'
    dataSource <- DataSourceText$new(dataSource = path)


    CorpusBuilderTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: CorpusBuilder: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    CorpusBuilderTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
raw <- test0()



}
className <- "CorpusBuilder"
#source('./test/unitTests/testCorpusBuilder.R')
testCorpusBuilder()
