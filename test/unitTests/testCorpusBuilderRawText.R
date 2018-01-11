testCorpusBuilder <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusBuilderTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: Create Data Source"
    cat(paste0("\n",test, " Commencing\n"))

    # Variables
    dataSource <- DataSourceText$new(dataSource = './test/testData/input')
    name <- 'Brown'
    path <- './test/testData/output'

    # Validation testing
    # builder <- CorpusBuilderRawText$new()  # OK
    # builder <- CorpusBuilderRawText$new(name = name) #ok
    # builder <- CorpusBuilderRawText$new(name = name, path = path) #ok
    # builder <- CorpusBuilderRawText$new(name = name, path = path, dataSource = stanford) #OK

    # Create corpus object
    builder <- CorpusBuilderRawText$new(name = name, path = path, dataSource = dataSource)
    builder <- builder$buildDocuments()
    documents <- builder$getDocuments()
    stopifnot(length(documents) == 3)
    stopifnot(length(documents[[1]]$getContent()) == 2000)
    stopifnot(length(documents[[2]]$getContent()) == 2000)
    stopifnot(length(documents[[3]]$getContent()) == 2000)
    brown <- builder$buildCorpus()$getResult()

    # Examine Brown Corpus
    content <- brown$getContent()
    stopifnot(length(content) == 3)
    stopifnot(length(content[[1]]) == 2000)
    stopifnot(length(content[[2]]) == 2000)
    stopifnot(length(content[[3]]) == 2000)

    # Check metadata
    print(brown$meta())
    print(brown$docMeta())


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
