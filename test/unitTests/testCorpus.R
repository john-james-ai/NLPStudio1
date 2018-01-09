testCorpus <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("foo", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("foo", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("bar", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("bar", ls(envir = .GlobalEnv))], envir = .GlobalEnv)

    foo <- FileCollection$new(name = 'foo', path = "./test/testData/input")
    return(foo)
  }

  test0 <- function(foo) {
    test <- "test0: Corpus: Instantiate and Import"
    cat(paste0("\n",test, " Commencing\n"))

    # Instantiate
    bar <- Corpus$new(name = 'bar', x = foo)
    b <- bar$exposeObject()
    stopifnot(b$name == 'bar')
    stopifnot(length(b$documents) == 3)

    # Get Document content
    documents <- bar$getDocuments()
    stopifnot(length(documents[[1]]$read()) > 1000)
    stopifnot(length(documents[[2]]$read()) > 1000)
    stopifnot(length(documents[[3]]$read()) > 1000)

    # Logit
    CorpusTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized set", b$name))
    cat(paste0(test, " Completed: Success!\n"))

    return(bar)
  }

  test1 <- function(bar) {
    test <- "test1: Corpus: Add and print metadata"
    cat(paste0("\n",test, " Commencing\n"))
    blogsDesc <- 'Blogs Register'
    newsDesc <- 'News Register'
    twitterDesc <- 'Twitter Register'

    bar$docMeta()
    bar$docMeta(key = 'title', value = c("Blogs", "News", "Twitter"))
    bar$docMeta(key = 'description', value = c(blogsDesc, newsDesc, twitterDesc))
    bar$corpusDocument0(key = "title", value = "Swiftkey Corpus")
    bar$corpusDocument0()
    bar$metaVarNames()

    # Logit
    CorpusTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized set", s$name))
    cat(paste0(test, " Completed: Success!\n"))

    return(bar)
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

foo <<- init()
bar <<- test0(foo)
bar <<- test1(bar)
# train <- test2(train)
# train <- test3(train)
# train <- test4(train)
}
className <- "Corpus"
CorpusTest <- LogTest$new()
#source('./test/unitTests/testCorpusBuilder.R')
testCorpus()
