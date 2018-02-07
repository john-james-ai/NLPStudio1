testCorpus <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
  }

  test0 <- function() {
    test <- "test0: Corpus: Create Corpus, add, get, remove documents"
    cat(paste0("\n",test, " Commencing\n"))

    blogsPath <- "./test/testData/input/en_US.blogs.txt"
    newsPath <- "./test/testData/input/en_US.news.txt"
    twitterPath <- "./test/testData/input/en_US.twitter.txt"

    # Create Documents Error Checking
    #blogs <- Document$new(name = 'blogs')$load(Document0)
    #news <- Document$new(name = 'news')$load("./j2j/test")
    #twitter <- Document$new(name = 'twitter')$load(path = blogsPath, io = Document0)

    # Create Documents
    blogs <- Document$new(name = 'blogs')
    news <- Document$new(name = 'news')
    twitter <- Document$new(name = 'twitter')

    # Check no content
    blogsText <- blogs$read()
    newsText <- news$read()
    twitterText <- twitter$read()
    stopifnot(length(blogsText) == 0)
    stopifnot(length(newsText) == 0)
    stopifnot(length(twitterText) == 0)

    # Check documents
    blogsText <- blogs$read(blogsPath)
    newsText <- news$read(newsPath)
    twitterText <- twitter$read(twitterPath)
    stopifnot(length(blogsText) == 2000)
    stopifnot(length(newsText) == 2000)
    stopifnot(length(twitterText) == 2000)


    # Create Corpus
    stanford <- Corpus$new(name = 'stanford')

    # Add Documents Error checking
    #stanford <- stanford$addDocument(Document0)
    #stanford <- stanford$addDocument(newsPath)
    #stanford <- stanford$addDocument()

    # Add Documents
    stanford <- stanford$addDocument(blogs)
    stanford <- stanford$addDocument(news)
    stanford <- stanford$addDocument(twitter)

    # Obtain content
    content <- stanford$read()
    stopifnot(length(content) == 3)

    # Get Documents
    documents <- stanford$getDocuments()
    stopifnot(length(documents) == 3)

    # Remove Documents Error Checking
    #stanford <- stanford$removeDocument(newsPath)
    #stanford <- stanford$removeDocument(Document0)
    #stanford <- stanford$removeDocument()

    # Remove Documents
    stanford <- stanford$removeDocument(blogs)
    stanford <- stanford$removeDocument(news)
    stanford <- stanford$removeDocument(twitter)

    documents <- stanford$getDocuments()
    stopifnot(length(documents) == 0)

    # Add documents back
    stanford <- stanford$addDocument(blogs)
    stanford <- stanford$addDocument(news)
    stanford <- stanford$addDocument(twitter)

    # Logit
    CorpusTest$logs(className = className, methodName = "initialize", msg = paste("Successfully instantiated Corpus object"))
    CorpusTest$logs(className = className, methodName = "addDocument", msg = paste("Successfully added Documents to Corpus object"))
    CorpusTest$logs(className = className, methodName = "getDocuments", msg = paste("Successfully obtained Document objects"))
    CorpusTest$logs(className = className, methodName = "removeDocument", msg = paste("Successfully removed a Document objects"))
    cat(paste0(test, " Completed: Success!\n"))

    return(stanford)
  }


  test1 <- function(stanford) {
    test <- "test1: Corpus: Document Metadata"
    cat(paste0("\n",test, " Commencing\n"))

    print(stanford$docMeta())
    stanford <- stanford$docMeta(key = "Title", value = c("Blogs", "News", "Twitter"))
    print(stanford$docMeta())

    # Check documents
    docs <- stanford$getDocuments()
    blogsMeta <- docs[[1]]$meta()
    newsMeta <- docs[[2]]$meta()
    twitMeta <- docs[[3]]$meta()

    # Check meta data for documents
    stopifnot(blogsMeta$title == "Blogs")
    stopifnot(newsMeta$title == "News")
    stopifnot(twitMeta$title == "Twitter")

    # Logit
    CorpusTest$logs(className = className, methodName = "initialize", msg = paste("Successfully instantiated Corpus object"))
    CorpusTest$logs(className = className, methodName = "addDocument", msg = paste("Successfully added Documents to Corpus object"))
    CorpusTest$logs(className = className, methodName = "getDocuments", msg = paste("Successfully obtained Document objects"))
    CorpusTest$logs(className = className, methodName = "removeDocument", msg = paste("Successfully removed a Document objects"))
    cat(paste0(test, " Completed: Success!\n"))

    return(stanford)
  }

  test2 <- function(stanford) {
    test <- "test2: Corpus: Write / Read Corpus"
    cat(paste0("\n",test, " Commencing\n"))

    path <- "./test/testData/writeCorpus"

    # Write Text
    stanford$write(path)
    content <- stanford$read()
    stopifnot(length(content) == 3)
    stopifnot(length(content[[1]]) == 2000)
    stopifnot(length(content[[2]]) == 2000)
    stopifnot(length(content[[3]]) == 2000)

    # Write Binary
    stanford$write(path, io = IOBin$new())
    content <- stanford$read(io = IOBin$new())
    stopifnot(length(content) == 3)
    stopifnot(length(content[[1]]) > 2000)
    stopifnot(length(content[[2]]) > 2000)
    stopifnot(length(content[[3]]) > 2000)

    # Logit
    CorpusTest$logs(className = className, methodName = "initialize", msg = paste("Successfully instantiated Corpus object"))
    CorpusTest$logs(className = className, methodName = "addDocument", msg = paste("Successfully added Documents to Corpus object"))
    CorpusTest$logs(className = className, methodName = "getDocuments", msg = paste("Successfully obtained Document objects"))
    CorpusTest$logs(className = className, methodName = "removeDocument", msg = paste("Successfully removed a Document objects"))
    cat(paste0(test, " Completed: Success!\n"))

    return(stanford)
  }


init()
stanford <<- test0()
stanford <<- test1(stanford)
stanford <<- test2(stanford)
}
className <- "Corpus"
CorpusTest <- LogTest$new()
testCorpus()
