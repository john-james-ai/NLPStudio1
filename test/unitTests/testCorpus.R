testCorpus <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("train", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("train", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("en_US.twitter", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.twitter", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("en_US.news", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.news", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("en_US.blogs", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.blogs", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    CorpusTest <<- LogTest$new()
    file.copy('./test/testData/hc/en_US.blogs.txt', 'test/testData/en_US.blogs.txt')
    file.copy('./test/testData/hc/en_US.news.txt', 'test/testData/en_US.news.txt')
    file.copy('./test/testData/hc/en_US.twitter.txt', 'test/testData/en_US.twitter.txt')
    blogs <- readLines(con = 'test/testData/en_US.blogs.txt')
    news <- readLines(con = 'test/testData/en_US.news.txt')
    twitter <- readLines(con = 'test/testData/en_US.twitter.txt')
    blogs <<- blogs[1:2000]
    news <<- news[1:2000]
    twitter <<- twitter[1:2000]
    writeLines(blogs, con = 'test/testData/en_US.blogs.txt')
    writeLines(news, con = 'test/testData/en_US.news.txt')
    writeLines(twitter, con = 'test/testData/en_US.twitter.txt')
    rm(textData)

  }

  test0 <- function() {
    test <- "test0: Corpus: Instantiate"
    cat(paste0("\n",test, " Commencing\n"))

    # Instantiate
    train <- Corpus$new(name = 'train', desc = 'Train Corpus')
    s <- train$exposeObject()
    stopifnot(s$className == 'Corpus')
    stopifnot(s$name == 'train')
    stopifnot(s$desc == 'Train Corpus')
    stopifnot(s$parent$getName() == 'nlpStudio')
    stopifnot(s$path == './NLPStudio/sets/train')
    stopifnot(length(s$documents) == 0)

    # Logit
    CorpusTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized set", s$name))
    cat(paste0(test, " Completed: Success!\n"))

    return(train)
  }

  test1 <- function() {
    test <- "test1: Corpus: Instantiate 2nd Corpus"
    cat(paste0("\n",test, " Commencing\n"))

    # Instantiate
    val <- Corpus$new(name = 'val', desc = 'val Corpus')
    s <- val$exposeObject()
    stopifnot(s$className == 'Corpus')
    stopifnot(s$name == 'val')
    stopifnot(s$desc == 'val Corpus')
    stopifnot(s$parent$getName() == 'nlpStudio')
    stopifnot(s$path == './NLPStudio/sets/val')
    stopifnot(length(s$documents) == 0)

    # Logit
    CorpusTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized set", s$name))
    cat(paste0(test, " Completed: Success!\n"))

    return(val)
  }

  test2 <- function(train) {
    test <- "test2: Corpus: Add Document"
    cat(paste0("\n",test, " Commencing\n"))

    # Instantiate new document
    news <- Document$new("./NLPStudio/documents/text/en_US.news.txt", desc = 'News Register')

    # Add document to training set
    train <- train$addDocument(news)
    s <- train$exposeObject()
    stopifnot(s$className == 'Corpus')
    stopifnot(s$name == 'train')
    stopifnot(s$desc == 'Train Corpus')
    stopifnot(s$parent$getName() == 'nlpStudio')
    stopifnot(s$path == './NLPStudio/sets/train')
    stopifnot(length(s$documents) == 1)
    stopifnot(s$documents$en_US.news.txt$getName() == 'en_US.news.txt')

    # Logit
    CorpusTest$logs(className = className, methodName = "addDocument", msg = paste("Successfully added", s$name, "to", train$getName()))
    cat(paste0(test, " Completed: Success!\n"))

    return(train)
  }

  test3 <- function(train) {
    test <- "test3: Corpus: Move Document"
    cat(paste0("\n",test, " Commencing\n"))

    # Get document
    news <- train$getDocuments()[[1]]

    # Move document
    news <- news$move(train)

    # Verify Document
    d <- news$exposeObject()
    stopifnot(d$className == 'Document')
    stopifnot(d$name == 'en_US.news.txt')
    stopifnot(d$desc == 'News Register')
    stopifnot(d$parent$getName() == 'train')
    stopifnot(d$path == './NLPStudio/sets/train/documents/text/en_US.news.txt')
    stopifnot(file.exists('./NLPStudio/sets/train/documents/text/en_US.news.txt'))

    # Logit
    CorpusTest$logs(className = className, methodName = "addDocument", msg = paste("Successfully moved", d$name, "to", d$parent$getName()))
    cat(paste0(test, " Completed: Success!\n"))

    return(train)
  }

  test4 <- function(train) {
    test <- "test4: Corpus: Remove Document"
    cat(paste0("\n",test, " Commencing\n"))

    # Get document
    train <- train$removeDocument(en_US.news.txt)

    # Verify Corpus
    s <- train$exposeObject()
    stopifnot(s$className == 'Corpus')
    stopifnot(s$name == 'train')
    stopifnot(s$desc == 'Train Corpus')
    stopifnot(s$parent$getName() == 'nlpStudio')
    stopifnot(s$path == './NLPStudio/sets/train')
    stopifnot(length(s$documents) == 0)
    stopifnot(file.exists('./NLPStudio/documents/text/en_US.news.txt'))

    # Logit
    CorpusTest$logs(className = className, methodName = "addDocument", msg = paste("Successfully removed", en_US.news.txt$getName(), "to", train$getName()))
    cat(paste0(test, " Completed: Success!\n"))

    return(train)
  }

init()
train <- test0()
test <- test1()
train <- test2(train)
train <- test3(train)
train <- test4(train)
}
className <- "Corpus"
CorpusTest <- LogTest$new()
#source('./test/unitTests/testCorpusBuilder.R')
testCorpus()
