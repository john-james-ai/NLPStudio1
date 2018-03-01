testPreprocessDocumentReshape <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("news", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("news", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    newsTxt <<- readLines("./test/testData/input/en_US.news.txt")
    PreprocessDocumentReshapeTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: Document: Instantiation document to repair"
    cat(paste0("\n",test, " Commencing\n"))

    # Validation
    # news <- Document$new()# should fail, no name

    # Instantiate
    news <- Document$new(name = 'news')
    news$text <- newsTxt

    # Update meta data
    news <- news$meta(key = "title", value = "News")
    news <- news$meta(key = "subject", value = "Technology News")
    news <- news$meta(key = "description", value = "News about technology and business")
    news <- news$meta(key = "language", value = "en")
    news <- news$meta(key = "creator", value = "Hans Christensen")
    news <- news$meta(key = "dateCreated", value = "12/22/2014")
    news <- news$meta(key = "source", value = "www.hc.com")
    news <- news$meta(key = "format", value = "txt")

    # Test instantiation
    d <- news$exposeObject()
    stopifnot(d$meta$name == 'news')
    stopifnot(d$meta$title == 'News')
    stopifnot(d$meta$description == "News about technology and business")
    stopifnot(d$meta$language == 'en')
    stopifnot(d$meta$creator == 'Hans Christensen')
    stopifnot(d$meta$dateCreated == '12/22/2014')
    stopifnot(d$meta$source == 'www.hc.com')
    stopifnot(d$meta$format == 'txt')

    # Logit
    PreprocessDocumentReshapeTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized document"))
    cat(paste0(test, " Completed: Success!\n"))

    return(news)
  }

  test1 <- function(news) {
    test <- "test1: Document: Reshape document"
    cat(paste0("\n",test, " Commencing\n"))

    #news2 <- PreprocessDocumentReshapeStrategy$new(news, "news2", unit = "sent")$preprocess()$getResult()# Error
    news2 <- PreprocessDocumentReshapeStrategy$new(news, "news2")$preprocess()$getResult()

    # Get news document data
    d <- news$exposeObject()
    d2 <- news2$exposeObject()
    stopifnot(d$meta$name == 'news')
    stopifnot(d2$meta$name == 'news2')
    stopifnot(d$meta$title == d2$meta$title)
    stopifnot(d$meta$description == d2$meta$description)
    stopifnot(d$meta$language == d2$meta$language)
    stopifnot(d$meta$creator == d2$meta$creator)
    stopifnot(d$meta$dateCreated == d2$meta$dateCreated)
    stopifnot(d$meta$source == d2$meta$source)
    stopifnot(d$meta$format == d2$meta$format)
    stopifnot(!identical(news$text, news2$text))

    # Check content
    c1 <- news$text
    c2 <- news2$text
    print(paste0("Length of old content is ", length(c1)))
    print(paste0("Length of new content is ", length(c2)))
    print("*******************************************")
    print(head(c1))
    print("*******************************************")
    print(head(c2))

    # Logit
    PreprocessDocumentReshapeTest$logs(className = className, methodName = "process", msg = paste("Successfully processed repair"))
    PreprocessDocumentReshapeTest$logs(className = className, methodName = "process", msg = paste("Successfully returned repaired object"))

    cat(paste0(test, " Completed: Success!\n"))

    return(news)
  }



  testn <- function(news) {
    test <- "Testn: Document: Write File"
    cat(paste0("\n",test, " Commencing\n"))

    # Logit
    PreprocessDocumentReshapeTest$logs(className = className, methodName = "content", msg = paste("Successfully instantiated document with file"))

    cat(paste0(test, " Completed: Success!\n"))

    return(news)
  }


init()
news <- test0()
news2 <- test1(news)

}
className <- "PreprocessDocumentReshape"

testPreprocessDocumentReshape()
