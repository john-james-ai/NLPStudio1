testSet <- function() {

  init <- function() {
    if (exists("train", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("train", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("val", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("val", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("en_US.news.txt", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.news.txt", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    file.copy('./test/testData/hc/en_US.news.txt', './NLPStudio/documents/text/en_US.news.txt')

  }

  test0 <- function() {
    test <- "test0: Set: Instantiate"
    cat(paste0("\n",test, " Commencing\n"))

    # Instantiate
    train <- Set$new(name = 'train', desc = 'Train Set')
    s <- train$exposeObject()
    stopifnot(s$className == 'Set')
    stopifnot(s$name == 'train')
    stopifnot(s$desc == 'Train Set')
    stopifnot(s$parent$getName() == 'nlpStudio')
    stopifnot(s$path == './NLPStudio/sets/train')
    stopifnot(length(s$documents) == 0)

    # Logit
    SetTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized set", s$name))
    cat(paste0(test, " Completed: Success!\n"))

    return(train)
  }

  test1 <- function() {
    test <- "test1: Set: Instantiate 2nd Set"
    cat(paste0("\n",test, " Commencing\n"))

    # Instantiate
    val <- Set$new(name = 'val', desc = 'val Set')
    s <- val$exposeObject()
    stopifnot(s$className == 'Set')
    stopifnot(s$name == 'val')
    stopifnot(s$desc == 'val Set')
    stopifnot(s$parent$getName() == 'nlpStudio')
    stopifnot(s$path == './NLPStudio/sets/val')
    stopifnot(length(s$documents) == 0)

    # Logit
    SetTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized set", s$name))
    cat(paste0(test, " Completed: Success!\n"))

    return(val)
  }

  test2 <- function(train) {
    test <- "test2: Set: Add Document"
    cat(paste0("\n",test, " Commencing\n"))

    # Instantiate new document
    news <- DocumentText$new("./NLPStudio/documents/text/en_US.news.txt", desc = 'News Register')

    # Add document to training set
    train <- train$addDocument(news)
    s <- train$exposeObject()
    stopifnot(s$className == 'Set')
    stopifnot(s$name == 'train')
    stopifnot(s$desc == 'Train Set')
    stopifnot(s$parent$getName() == 'nlpStudio')
    stopifnot(s$path == './NLPStudio/sets/train')
    stopifnot(length(s$documents) == 1)
    stopifnot(s$documents$en_US.news.txt$getName() == 'en_US.news.txt')

    # Logit
    SetTest$logs(className = className, methodName = "addDocument", msg = paste("Successfully added", s$name, "to", train$getName()))
    cat(paste0(test, " Completed: Success!\n"))

    return(train)
  }

  test3 <- function(train) {
    test <- "test3: Set: Move Document"
    cat(paste0("\n",test, " Commencing\n"))

    # Get document
    news <- train$getDocuments()[[1]]

    # Move document
    news <- news$move(train)

    # Verify Document
    d <- news$exposeObject()
    stopifnot(d$className == 'DocumentText')
    stopifnot(d$name == 'en_US.news.txt')
    stopifnot(d$desc == 'News Register')
    stopifnot(d$parent$getName() == 'train')
    stopifnot(d$path == './NLPStudio/sets/train/documents/text/en_US.news.txt')
    stopifnot(file.exists('./NLPStudio/sets/train/documents/text/en_US.news.txt'))

    # Logit
    SetTest$logs(className = className, methodName = "addDocument", msg = paste("Successfully moved", d$name, "to", d$parent$getName()))
    cat(paste0(test, " Completed: Success!\n"))

    return(train)
  }

  test4 <- function(train) {
    test <- "test4: Set: Remove Document"
    cat(paste0("\n",test, " Commencing\n"))

    # Get document
    train <- train$removeDocument(en_US.news.txt)

    # Verify Set
    s <- train$exposeObject()
    stopifnot(s$className == 'Set')
    stopifnot(s$name == 'train')
    stopifnot(s$desc == 'Train Set')
    stopifnot(s$parent$getName() == 'nlpStudio')
    stopifnot(s$path == './NLPStudio/sets/train')
    stopifnot(length(s$documents) == 0)
    stopifnot(file.exists('./NLPStudio/documents/text/en_US.news.txt'))

    # Logit
    SetTest$logs(className = className, methodName = "addDocument", msg = paste("Successfully removed", en_US.news.txt$getName(), "to", train$getName()))
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
className <- "Set"
SetTest <- LogTest$new()
#source('./test/unitTests/testCorpusBuilder.R')
testSet()
