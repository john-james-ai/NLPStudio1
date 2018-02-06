testTextSalon <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    TextSalonTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: TextSalon"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from directory source
    name <- "Corpus"
    desc <- "Creating corpus from directory sources"
    dataSource <- "./test/testData/fast"
    corpus <- CorpusImportDir$new(name, dataSource)$build()$getResult()
    ts <- TextSalon$new(corpus)
    acs <- AddCommaSpace$new()
    ame <- AddEndMark$new()
    re <- RemoveEmail$new()
    rh <- RemoveHyphens$new()
    rn <- RemoveNumbers$new()
    ts <- ts$addCommand(acs)
    ts <- ts$addCommand(ame)
    ts <- ts$addCommand(re)
    ts <- ts$addCommand(rh)
    ts <- ts$addCommand(rn)
    corpus2 <- ts$execute()$getResult()
    docs <- corpus2$getDocuments()
    print(head(docs[[1]]$content, 2))
    print(head(docs[[2]]$content, 2))
    print(head(docs[[3]]$content, 2))



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
