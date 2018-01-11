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
    text <- DataSourceText$new(dataSource = dataSource)$getSource()
    stopifnot(length(text) == 3)
    stopifnot(length(text[[1]]) == 2000)
    stopifnot(length(text[[2]]) == 2000)
    stopifnot(length(text[[3]]) == 2000)
    stopifnot(names(text) == docNames)


    # Test filename
    dataSource <- "./test/testData/input/en_US.blogs.txt"
    text <- DataSourceText$new(dataSource = dataSource)$getSource()
    stopifnot(length(text) == 2000)
    stopifnot(names(text) == "en_US.blogs")

    # Test List
    dataSource <- content
    text <- DataSourceText$new(dataSource = dataSource)$getSource()
    stopifnot(length(text) == 3)
    stopifnot(length(text[[1]]) == 2000)
    stopifnot(length(text[[2]]) == 2000)
    stopifnot(length(text[[3]]) == 2000)
    stopifnot(names(text) == docNames)

    # Test List (Unnamed)
    docNames = c("Document1", "Document2", "Document3")
    dataSource <- content2
    text <- DataSourceText$new(dataSource = dataSource)$getSource()
    stopifnot(length(text) == 3)
    stopifnot(length(text[[1]]) == 2000)
    stopifnot(length(text[[2]]) == 2000)
    stopifnot(length(text[[3]]) == 2000)
    stopifnot(names(text) == docNames)

    # Test vector
    dataSource <- news
    text <- DataSourceText$new(dataSource = dataSource)$getSource()
    stopifnot(length(text) == 2000)
    stopifnot(names(text) == "en_US.news")

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
