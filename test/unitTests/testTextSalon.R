testTextSalon <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    TextSalonTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: TextSalon: Directory"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from directory source
    name <- "Corpus"
    desc <- "Creating corpus from directory sources"
    dataSource <- "./test/testData/input"
    corpus <- CorpusImportDir$new(name, dataSource)$build()$getResult()
    ts <- TextSalon$new(corpus)
    cmd <- AddCommaSpace$new(corpus)
    corpus2 <- ts$addCommand(cmd)$execute()$getResults()


    TextSalonTest$logs(className = "TextSalon", methodName = "initiate", msg = paste("Successfully instantiated. "))
    TextSalonTest$logs(className = "TextSalon", methodName = "build", msg = paste("Successfully instantiated. "))
    TextSalonTest$logs(className = "TextSalon", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }

  test1 <- function() {
    test <- "test1: TextSalon: WildCard"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from Vector Flat
    name <- "TextSalon"
    desc <- "Creating corpus from directory sources"
    dataSource <- "./test/testData/input/*s.txt"
    TextSalon <- TextSalon$new(name, dataSource)$build()$getResult()
    TextSalonContent <- TextSalon$read()
    stopifnot(length(TextSalonContent) == 2)
    TextSalonDocuments <- TextSalon$getDocuments()
    stopifnot(length(TextSalonDocuments) == 2)
    TextSalon$meta(key = "desc", value = desc)
    TextSalon$docMeta(key = "year", value = "2018")
    print(TextSalon$meta())
    print(TextSalon$docMeta())

    TextSalonTest$logs(className = "TextSalon", methodName = "initiate", msg = paste("Successfully instantiated. "))
    TextSalonTest$logs(className = "TextSalon", methodName = "build", msg = paste("Successfully instantiated. "))
    TextSalonTest$logs(className = "TextSalon", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: TextSalon: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    TextSalonTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
test0()
test1()


}
className <- "TextSalon"
#source('./test/unitTests/testTextSalon.R')
testTextSalon()
