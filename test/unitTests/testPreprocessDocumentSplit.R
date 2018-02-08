testPreprocessDocumentSplitStrategy <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("twitter", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("twitter", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    tweets <- readLines("./test/testData/input/en_US.twitter.txt")
    PreprocessDocumentSplitStrategyTest <<- LogTest$new()
    return(tweets)
  }

  test0 <- function(tweets) {
    test <- "test0: Document: Create Document"
    cat(paste0("\n",test, " Commencing\n"))

    twitter <- Document$new(name = "twitter")
    twitter$content <- tweets
    print(paste0("Length of old content is ", length(twitter$content)))

    # Reshape data
    twitter <<- PreprocessDocumentReshapeStrategy$new(object = twitter)$preprocess()$getResult()
    print(paste0("Length of reshaped content is ", length(twitter$content)))

    # Update meta data
    twitter <- twitter$meta(key = "title", value = "Tweets")
    twitter <- twitter$meta(key = "subject", value = "Technology Tweets")
    twitter <- twitter$meta(key = "description", value = "News about technology and tweets")
    twitter <- twitter$meta(key = "language", value = "en")
    twitter <- twitter$meta(key = "creator", value = "Hans Christensen")
    twitter <- twitter$meta(key = "dateCreated", value = "12/22/2014")
    twitter <- twitter$meta(key = "source", value = "www.hc.com")
    twitter <- twitter$meta(key = "format", value = "txt")

    # Logit
    PreprocessDocumentSplitStrategyTest$logs(className = "Document", methodName = "initialize", msg = paste("Successfully initialized document"))
    PreprocessDocumentSplitStrategyTest$logs(className = "Document", methodName = "content", msg = paste("Successfully set content"))
    PreprocessDocumentSplitStrategyTest$logs(className = "Document", methodName = "meta", msg = paste("Successfully set meta data"))
    cat(paste0(test, " Completed: Success!\n"))

    return(twitter)
  }

  test1 <- function(twitter) {
    test <- "test1: Document: Validation"
    cat(paste0("\n",test, " Commencing\n"))

    #splits <- PreprocessDocumentSplitStrategy$new() # Object missing
    #splits <- PreprocessDocumentSplitStrategy$new(object = twitter, trainSize = 2, testSize = 2) # Invalid sizes

    # Logit
    PreprocessDocumentSplitStrategyTest$logs(className = className, methodName = "initialize", msg = paste("Successfully validated input"))

    cat(paste0(test, " Completed: Success!\n"))

    return(twitter)
  }


  test2 <- function(twitter) {
    test <- "test2: Document: Two-way Split"
    cat(paste0("\n",test, " Commencing\n"))

    # Split data
    splits <- PreprocessDocumentSplitStrategy$new(object = twitter, trainSize = .8, testSize = .2)$preprocess()$getResult()
    stopifnot(length(splits) == 2)

    # Check meta data
    train <- splits[["train"]]
    testSet <- splits[["test"]]
    stopifnot(twitter$meta() == train$meta())
    stopifnot(twitter$meta() == testSet$meta())


    #Print head of each file
    print("Training Set")
    print(head(train$content))
    print("Test Set")
    print(head(testSet$content))

    # Check text length
    print(paste0("Length of training set is ", length(train$content)))
    print(paste0("Length of test set is ", length(testSet$content)))

    # Logit
    PreprocessDocumentSplitStrategyTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized split"))
    PreprocessDocumentSplitStrategyTest$logs(className = className, methodName = "preprocess", msg = paste("Successfully executed split"))
    PreprocessDocumentSplitStrategyTest$logs(className = className, methodName = "getResult", msg = paste("Successfully returned repaired object"))

    cat(paste0(test, " Completed: Success!\n"))

    return(splits)
  }

  test3 <- function(twitter) {
    test <- "test3: Document: 3-way Split"
    cat(paste0("\n",test, " Commencing\n"))

    # Split data
    splits <- PreprocessDocumentSplitStrategy$new(object = twitter, trainSize = .6, valSize = .2,testSize = .2)$preprocess()$getResult()
    stopifnot(length(splits) == 3)

    # Check meta data
    train <- splits[["train"]]
    val <- splits[["validation"]]
    testSet <- splits[["test"]]
    stopifnot(twitter$meta() == train$meta())
    stopifnot(twitter$meta() == val$meta())
    stopifnot(twitter$meta() == testSet$meta())

    # Check text length
    print(paste0("Length of training set is ", length(train$content)))
    print(paste0("Length of validation set is ", length(val$content)))
    print(paste0("Length of test set is ", length(testSet$content)))

    #Print head of each file
    print("Training Set")
    print(head(train$content))
    print("Validation Set")
    print(head(val$content))
    print("Test Set")
    print(head(testSet$content))

    # Logit
    PreprocessDocumentSplitStrategyTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized split"))
    PreprocessDocumentSplitStrategyTest$logs(className = className, methodName = "preprocess", msg = paste("Successfully executed split"))
    PreprocessDocumentSplitStrategyTest$logs(className = className, methodName = "getResult", msg = paste("Successfully returned repaired object"))

    cat(paste0(test, " Completed: Success!\n"))

    return(splits)
  }

  testn <- function(twitter) {
    test <- "Testn: Document: Write File"
    cat(paste0("\n",test, " Commencing\n"))

    # Logit
    PreprocessDocumentSplitStrategyTest$logs(className = className, methodName = "content", msg = paste("Successfully instantiated document with file"))

    cat(paste0(test, " Completed: Success!\n"))

    return(twitter)
  }


tweets <- init()
twitter <- test0(tweets)
twitter <- test1(twitter)
splits <- test2(twitter)
splits <<- test3(twitter)

}
className <- "PreprocessDocumentSplitStrategy"

testPreprocessDocumentSplitStrategy()
