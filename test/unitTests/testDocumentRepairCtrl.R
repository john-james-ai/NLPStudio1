testDocumentRepairCtrl <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("news", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("news", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    newsTxt <<- readLines("./test/testData/hc/en_US.news.txt")
    RepairDocumentCtrlTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: Document: Instantiation document to repair"
    cat(paste0("\n",test, " Commencing\n"))

    # Validation
    # news <- Document$new()# should fail, no name

    # Instantiate
    news <- Document$new(name = 'news')
    news$content <- newsTxt

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
    RepairDocumentCtrlTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized document"))
    cat(paste0(test, " Completed: Success!\n"))

    return(news)
  }

  test1 <- function(news) {
    test <- "test1: Document: Repair document with defaults"
    cat(paste0("\n",test, " Commencing\n"))

    news2 <- ProcessDocumentRepairCtrl$new(news, "news2")$process()$getResult()

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
    stopifnot(!identical(news$content, news2$content))

    # Check content
    c1 <- news$content
    c2 <- news2$content
    print(paste0("Length of old content is ", length(c1)))
    print(paste0("Length of new content is ", length(c2)))

    # Logit
    RepairDocumentCtrlTest$logs(className = className, methodName = "process", msg = paste("Successfully processed repair"))
    RepairDocumentCtrlTest$logs(className = className, methodName = "process", msg = paste("Successfully returned repaired object"))

    cat(paste0(test, " Completed: Success!\n"))

    return(news)
  }


  test2 <- function(news) {
    test <- "test2: Document: Repair with Custom by Decimal Code"
    cat(paste0("\n",test, " Commencing\n"))

    # Initialize parameters
    pattern = c(0,1,3,5,7,9,24,26, 127)
    replace = c(0x20, 0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20)
    subs = data.frame(pattern, replace)

    news3 <- ProcessDocumentRepairCtrl$new(news2, "news3", substitutions = subs)$process()$getResult()

    # Get news document data
    d2 <- news2$exposeObject()
    d3 <- news3$exposeObject()
    stopifnot(d2$meta$name == 'news')
    stopifnot(d3$meta$name == 'news3')
    stopifnot(d3$meta$title == d2$meta$title)
    stopifnot(d3$meta$description == d2$meta$description)
    stopifnot(d3$meta$language == d2$meta$language)
    stopifnot(d3$meta$creator == d2$meta$creator)
    stopifnot(d3$meta$dateCreated == d2$meta$dateCreated)
    stopifnot(d3$meta$source == d2$meta$source)
    stopifnot(d3$meta$format == d2$meta$format)
    stopifnot(!identical(news2$content, news3$content))

    # Check content
    c2 <- news2$content
    c3 <- news3$content
    print(paste0("Length of old content is ", length(c2)))
    print(paste0("Length of new content is ", length(c3)))

    # Logit
    RepairDocumentCtrlTest$logs(className = className, methodName = "process", msg = paste("Successfully processed repair"))
    RepairDocumentCtrlTest$logs(className = className, methodName = "process", msg = paste("Successfully returned repaired object"))

    cat(paste0(test, " Completed: Success!\n"))

    return(news3)
  }

  testn <- function(news) {
    test <- "Testn: Document: Write File"
    cat(paste0("\n",test, " Commencing\n"))

    # Logit
    RepairDocumentCtrlTest$logs(className = className, methodName = "content", msg = paste("Successfully instantiated document with file"))

    cat(paste0(test, " Completed: Success!\n"))

    return(news)
  }


init()
news <- test0()
news2 <- test1(news)
news3 <- test2(news)

}
className <- "ProcessDocumentRepairCtrl"

testDocumentRepairCtrl()
