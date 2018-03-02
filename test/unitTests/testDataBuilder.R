testDataBuilder <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("builder", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("builder", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("director", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("director", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("corpusSource", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("corpusSource", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("cSource", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("cSource", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("data", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("data", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    unlink("./test/testData/data", recursive = TRUE)
    DataBuilderTest <<- LogTest$new()
  }



  test0 <- function() {
    test <- "test0: DataBuilder: Source Data"
    cat(paste0("\n",test, " Commencing\n"))

    # Create director and builder
    name <- 'data'
    path <- './test/testData/data'
    director <- DataDirector$new()
    builder <- DataBuilder$new(name = name, path = path)

    # Create data source
    name <- 'raw'
    path <- "./test/testData/data/raw"
    params <- list()
    params[['url']] <- 'http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
    params[['zipFiles']] <- c(file.path('final/en_US/en_US.blogs.txt'),
                              file.path('final/en_US/en_US.news.txt'),
                              file.path('final/en_US/en_US.twitter.txt'))
    corpusSource <- BuildDataRawWebZip$new(name = name, path = path, params = params)

    # Create commands
    cSource <- CSourceData$new(builder, corpusSource)
    cRepair <- CRepair$new(builder, )
    director <- director$addCommand(cSource)

    # Create


    data <- director$execute()

    DataBuilderTest$logs(className = 'DataDirector', methodName = "initiate", msg = paste("Successfully instantiated DataDirector. "))
    DataBuilderTest$logs(className = 'DataBuilder', methodName = "initiate", msg = paste("Successfully instantiated DataBuilder and Data object "))
    DataBuilderTest$logs(className = 'DataDirector', methodName = "addCommand", msg = paste("Successfully added a command to DataDirector. "))
    DataBuilderTest$logs(className = 'DataDirector', methodName = "execute", msg = paste("Successfully executed command in DataDirector. "))

    cat(paste0(test, " Completed: Success!\n"))

    return(data)
  }



  testn <- function() {
    test <- "testn: DataBuilder: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    DataBuilderTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testData/swiftKey/data/external"

init()
raw <- test0()



}
className <- "DataBuilder"
#source('./test/unitTests/testCorpusBuilder.R')
testDataBuilder()
