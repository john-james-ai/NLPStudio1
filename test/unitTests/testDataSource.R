testDataSource <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    DataSourceTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: DataSourceText"
    cat(paste0("\n",test, " Commencing\n"))

    # Test Directory
    dataSource <- "./test/testData/input"
    docNames = c("en_US.blogs", "en_US.news", "en_US.twitter")
    textDir <- DataSourceText$new(dataSource = dataSource)$getSource()
    stopifnot(length(textDir) == 3)
    stopifnot(length(textDir[[1]]) == 2000)
    stopifnot(length(textDir[[2]]) == 2000)
    stopifnot(length(textDir[[3]]) == 2000)
    stopifnot(names(textDir) == docNames)


    # Test filename
    dataSource <- "./test/testData/input/en_US.blogs.txt"
    textFile <- DataSourceText$new(dataSource = dataSource)$getSource()
    stopifnot(length(textFile) == 2000)
    stopifnot(names(textFile) == "en_US.blogs")

    # Test List
    dataSource <- content
    textList <- DataSourceText$new(dataSource = dataSource)$getSource()
    stopifnot(length(textList) == 3)
    stopifnot(length(textList[[1]]) == 2000)
    stopifnot(length(textList[[2]]) == 2000)
    stopifnot(length(textList[[3]]) == 2000)
    stopifnot(names(textList) == docNames)

    # Test List (Unnamed)
    docNames = c("Document1", "Document2", "Document3")
    dataSource <- content2
    textUnnamedList <- DataSourceText$new(dataSource = dataSource)$getSource()
    stopifnot(length(textUnnamedList) == 3)
    stopifnot(length(textUnnamedList[[1]]) == 2000)
    stopifnot(length(textUnnamedList[[2]]) == 2000)
    stopifnot(length(textUnnamedList[[3]]) == 2000)
    stopifnot(names(textUnnamedList) == docNames)

    # Test vector
    dataSource <- news
    textVec <- DataSourceText$new(dataSource = dataSource)$getSource()
    stopifnot(length(textVec) == 2000)
    stopifnot(names(textVec) == "en_US.news")

    # Logit
    DataSourceTest$logs(className = className, methodName = "initialize", msg = paste0(test, ", Successfully!"))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }




init()
test0()

}
className <- "DataSource"

testDataSource()
