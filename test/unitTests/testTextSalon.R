testTextSalon <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
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
    acs <- AddCommaSpace$new()
    ame <- AddEndMark$new()
    ts <- ts$addCommand(acs)
    ts <- ts$addCommand(ame)
    corpus2 <- ts$execute()$getResult()



    TextSalonTest$logs(className = "TextSalon", methodName = "initiate", msg = paste("Successfully instantiated. "))
    TextSalonTest$logs(className = "TextSalon", methodName = "execute", msg = paste("Processing successfully executed."))
    TextSalonTest$logs(className = "TextSalon", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return(corpus2)
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
corpus2 <<- test0()


}
className <- "TextSalon"
#source('./test/unitTests/testTextSalon.R')
testTextSalon()
