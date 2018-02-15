testSplitCorpus <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    SplitCorpusTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: Corpus: Create Corpus"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from directory source
    name <- "CorpusSourceDir"
    desc <- "Creating corpus from directory sources"
    dataSource <- "./test/testData/input"
    corpus <- CorpusSourceDir$new(name, dataSource)$build()$getResult()
    corpusContent <- corpus$read()
    stopifnot(length(corpusContent) == 3)
    docs <- corpus$getDocuments()
    stopifnot(length(docs) == 3)
    lapply(docs, function(d) {
      name <- d$getName()
      print(paste("Length of original", name, " content is:", length(d$content)))
    })
    
    # Preprocess
    ts <- TextStudio$new(corpus)
    cmd <- TokenizeCmd$new(what = "sentence")
    ts <- ts$addCommand(cmd)
    corpus <- ts$execute()$getResult()
    

    # Create meta data
    corpus <- corpus$meta(key = "title", value = "SF Corpus")
    corpus <- corpus$meta(key = "subject", value = "Parties, Music, Art")
    corpus <- corpus$meta(key = "description", value = "Blogs, news, tweets re parties, music and art")
    corpus <- corpus$meta(key = "language", value = "en")
    corpus <- corpus$meta(key = "creator", value = "John")
    corpus <- corpus$meta(key = "dateCreated", value = "12/22/2014")
    corpus <- corpus$meta(key = "source", value = "www.hc.com")
    corpus <- corpus$meta(key = "format", value = "txt")
    corpus <- corpus$docMeta(key = "name", value = c("Party", "Dance", "Love"))

    # Logit
    SplitCorpusTest$logs(className = "CorpusSourceDir", methodName = "all", msg = paste("Imported Corpus"))
    SplitCorpusTest$logs(className = "ProcessCorpusRehapeStrategy", methodName = "all", msg = paste("Successfully reshaped content"))
    cat(paste0(test, " Completed: Success!\n"))

    return(corpus)
  }
  
  test1 <- function(corpus) {
    test <- "test1: Corpus: Validation"
    cat(paste0("\n",test, " Commencing\n"))

    #splits <- SplitCorpus$new() # Object missing
    #splits <- SplitCorpus$new(x = corpus, trainSize = 2, testSize = 2) # Invalid sizes

    # Logit
    SplitCorpusTest$logs(className = className, methodName = "initialize", msg = paste("Successfully validated input"))

    cat(paste0(test, " Completed: Success!\n"))

    return(corpus)
  }


  test2 <- function(corpus) {
    test <- "test2: Corpus: Two-way Split"
    cat(paste0("\n",test, " Commencing\n"))

    # Split data
    splits <- SplitCorpus$new(x = corpus, trainSize = .8, testSize = .2)$execute()$getResult()
    stopifnot(length(splits) == 2)

    # Unsplit splits
    trainCorpus <- splits[['train']]
    testCorpus <- splits[['test']]

    # Get documents
    origDocs <- corpus$getDocuments()
    trainDocs <- trainCorpus$getDocuments()
    testDocs <- testCorpus$getDocuments()

    # Check meta data
    stopifnot(corpus$meta() == trainCorpus$meta())
    stopifnot(corpus$meta() == testCorpus$meta())
    lapply(seq_along(trainDocs), function(d) {
      stopifnot(origDocs[[d]]$meta() == trainDocs[[d]]$meta())
      stopifnot(origDocs[[d]]$meta() == testDocs[[d]]$meta())
    })

    # Check document contents
    lapply(seq_along(origDocs), function(d) {
      print(paste("     Analyzing document:", origDocs[[d]]$getName()))
      print(paste("Original content length:", length(origDocs[[d]]$content)))
      print(paste("Training content length:", length(trainDocs[[d]]$content)))
      print(paste("    Test content length:", length(testDocs[[d]]$content)))
    })

    # Logit
    SplitCorpusTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized split"))
    SplitCorpusTest$logs(className = className, methodName = "execute", msg = paste("Successfully executed split"))
    SplitCorpusTest$logs(className = className, methodName = "getResult", msg = paste("Successfully returned repaired object"))

    cat(paste0(test, " Completed: Success!\n"))

    return(splits)
  }

  test3 <- function(twitter) {
    test <- "test3: Corpus: 3-way Split"
    cat(paste0("\n",test, " Commencing\n"))

    # Split data
    splits <- SplitCorpus$new(x = corpus, trainSize = .6, valSize = .2, testSize = .2)$execute()$getResult()
    stopifnot(length(splits) == 3)

    # Unsplit splits
    trainCorpus <- splits[['train']]
    valCorpus <- splits[["validation"]]
    testCorpus <- splits[['test']]

    # Get documents
    origDocs <- corpus$getDocuments()
    trainDocs <- trainCorpus$getDocuments()
    valDocs <- valCorpus$getDocuments()
    testDocs <- testCorpus$getDocuments()

    # Check meta data
    stopifnot(corpus$meta() == trainCorpus$meta())
    stopifnot(corpus$meta() == valCorpus$meta())
    stopifnot(corpus$meta() == testCorpus$meta())
    lapply(seq_along(trainDocs), function(d) {
      stopifnot(origDocs[[d]]$meta() == trainDocs[[d]]$meta())
      stopifnot(origDocs[[d]]$meta() == valDocs[[d]]$meta())
      stopifnot(origDocs[[d]]$meta() == testDocs[[d]]$meta())
    })

    # Check document contents
    lapply(seq_along(origDocs), function(d) {
      print(paste("\nAnalyzing document:", origDocs[[d]]$getName()))
      print(paste("  Original content length:", length(origDocs[[d]]$content)))
      print(paste("  Training content length:", length(trainDocs[[d]]$content)))
      print(paste("Validation content length:", length(valDocs[[d]]$content)))
      print(paste("      Test content length:", length(testDocs[[d]]$content)))
    })

    # Logit
    SplitCorpusTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized split"))
    SplitCorpusTest$logs(className = className, methodName = "execute", msg = paste("Successfully executed split"))
    SplitCorpusTest$logs(className = className, methodName = "getResult", msg = paste("Successfully returned repaired object"))

    cat(paste0(test, " Completed: Success!\n"))

    return(splits)
  }

  testn <- function(twitter) {
    test <- "Testn: Corpus: Write File"
    cat(paste0("\n",test, " Commencing\n"))

    # Logit
    SplitCorpusTest$logs(className = className, methodName = "content", msg = paste("Successfully instantiated document with file"))

    cat(paste0(test, " Completed: Success!\n"))

    return(twitter)
  }


init()
corpus <- test0()
corpus <- test1(corpus)
splits <- test2(corpus)
splits <<- test3(corpus)

}
className <- "SplitCorpus"

  testSplitCorpus()
