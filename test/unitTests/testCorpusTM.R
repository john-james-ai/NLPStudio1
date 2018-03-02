testCorpusTM <- function() {

  init <- function() {
    library(tm)
    source('./test/testFunctions/LogTest.R')
    unlink("./test/testCorpus/data", recursive = TRUE)
    CorpusTMTest <<- LogTest$new()
    tmc <- tm::VCorpus(tm::DirSource("./test/testData/input"))
    tm::meta(tmc, tag = "2018", type = "corpus") <- "Corpus created using a TM VCorpus"
    tm::meta(tmc, tag = "desc", type = "corpus") <- "Corpus created using a TM VCorpus"
    tm::meta(tmc, tag = "name", type = "indexed") <- c("Blogs", "News", "Twitter")
    return(tmc)
  }

  test0 <- function(tmc) {
    test <- "test0: CorpusTM"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from directory source
    name <- "corpusTM"
    desc <- "Creating corpus from TM corpus object"
    docDesc <- c("Blogs Data", "News of the world", "Tweets and rants")
    corpusSource <- tmc
    corpusTM <- CorpusTM$new(name, corpusSource)$build()$getResult()
    corpusTMContent <- corpusTM$read()
    stopifnot(length(corpusTMContent) == 3)

    corpusTMDocuments <- corpusTM$getDocuments()
    stopifnot(length(corpusTMDocuments) == 3)
    corpusTM$meta(key = "desc", value = desc)
    corpusTM$docMeta(key = "desc", value = docDesc)
    print(corpusTM$meta())
    print(corpusTM$docMeta())

    CorpusTMTest$logs(className = "CorpusTM", methodName = "initiate", msg = paste("Successfully instantiated. "))
    CorpusTMTest$logs(className = "CorpusTM", methodName = "build", msg = paste("Successfully instantiated. "))
    CorpusTMTest$logs(className = "CorpusTM", methodName = "getResult", msg = paste("Successfully returned corpus. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


  testn <- function() {
    test <- "testn: CorpusTM: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    CorpusTMTest$logs(className = className, methodName = "initiate", msg = paste("Successfully instantiated file collection. "))
    cat(paste0(test, " Completed: Success!\n"))

    return()
  }

tmc <- init()
test0(tmc)


}
className <- "CorpusTM"
#source('./test/unitTests/testCorpusTM.R')
testCorpusTM()
