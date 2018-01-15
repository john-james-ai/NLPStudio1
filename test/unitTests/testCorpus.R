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
    blogs <- Document$new(name = 'blogs')$load(blogsPath)
    news <- Document$new(name = 'news')$load(newsPath)
    twitter <- Document$new(name = 'twitter')$load(twitterPath)

    # Check documents
    blogsText <- blogs$read()
    newsText <- news$read()
    twitterText <- twitter$read()
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
    test <- "test0: Corpus: Document Metadata"
    cat(paste0("\n",test, " Commencing\n"))

    print(stanford$docMeta())
    stanford <- stanford$docMeta(key = "Title", value = c("Blogs", "News", "Twitter"))
    print(stanford$docMeta())

    # Logit
    CorpusTest$logs(className = className, methodName = "initialize", msg = paste("Successfully instantiated Corpus object"))
    CorpusTest$logs(className = className, methodName = "addDocument", msg = paste("Successfully added Documents to Corpus object"))
    CorpusTest$logs(className = className, methodName = "getDocuments", msg = paste("Successfully obtained Document objects"))
    CorpusTest$logs(className = className, methodName = "removeDocument", msg = paste("Successfully removed a Document objects"))
    cat(paste0(test, " Completed: Success!\n"))

    return(stanford)
  }
  #TODO: Test meta data
init()
stanford <<- test0()
stanford <<- test1(stanford)

}
className <- "Corpus"
CorpusTest <- LogTest$new()
testCorpus()
