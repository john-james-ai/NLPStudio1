testPreprocessCorpusBin <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("news", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("news", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    newsTxt <<- readLines("./test/testData/hc/en_US.news.txt")
    PreprocessCorpusBinTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: Corpus: Instantiation corpus to repair"
    cat(paste0("\n",test, " Commencing\n"))

    # Build Corpus from directory source
    name <- "CorpusImportDir"
    desc <- "Creating corpus from directory sources"
    dataSource <- "./test/testData/input"
    cid <- CorpusImportDir$new(name, dataSource)$build()$getResult()
    cidContent <- cid$read()
    stopifnot(length(cidContent) == 3)
    cidDocuments <- cid$getDocuments()
    stopifnot(length(cidDocuments) == 3)
    cid$meta(key = "desc", value = desc)
    cid$docMeta(key = "year", value = "2018")
    print(cid$meta())
    print(cid$docMeta())

    # Update meta data
    cid <- cid$meta(key = "title", value = "Berkeley Corpus")
    cid <- cid$meta(key = "subject", value = "Berkeley Corpus from Directory")
    cid <- cid$meta(key = "description", value = "Blogs, news and tweets for twits")
    cid <- cid$meta(key = "language", value = "en")
    cid <- cid$meta(key = "creator", value = "Hans Christensen")
    cid <- cid$meta(key = "dateCreated", value = "12/22/2014")
    cid <- cid$meta(key = "source", value = "www.hc.com")
    cid <- cid$meta(key = "format", value = "txt")

    # Test instantiation
    b <- cid$exposeObject()
    stopifnot(b$meta$name == 'cid')
    stopifnot(b$meta$title == 'Berkeley Corpus')
    stopifnot(b$meta$description == "Blogs, news and tweets for twits")
    stopifnot(b$meta$language == 'en')
    stopifnot(b$meta$creator == 'Hans Christensen')
    stopifnot(b$meta$dateCreated == '12/22/2014')
    stopifnot(b$meta$source == 'www.hc.com')
    stopifnot(b$meta$format == 'txt')

    # Logit
    PreprocessCorpusBinTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized Corpus"))
    cat(paste0(test, " Completed: Success!\n"))

    return(cid)
  }

  test1 <- function(cb) {
    test <- "test1: Corpus: Repair document with defaults"
    cat(paste0("\n",test, " Commencing\n"))

    ct <- PreprocessCorpusBin$new(cb, "Caltech")$preprocess()$getResult()

    # Get news Corpus meta data
    c1 <- cb$exposeObject()
    c2 <- ct$exposeObject()
    stopifnot(c1$meta$name == 'Berkeley')
    stopifnot(c2$meta$name == 'Caltech')
    stopifnot(c1$meta$title == c2$meta$title)
    stopifnot(c1$meta$description == c2$meta$description)
    stopifnot(c1$meta$language == c2$meta$language)
    stopifnot(c1$meta$creator == c2$meta$creator)
    stopifnot(c1$meta$dateCreated == c2$meta$dateCreated)
    stopifnot(c1$meta$source == c2$meta$source)
    stopifnot(c1$meta$format == c2$meta$format)

    docs1 <- cb$getDocuments()
    docs2 <- ct$getDocuments()

    lapply(seq_along(docs1), function(d) {
      t1 <- docs1[[d]]$content
      t2 <- docs2[[d]]$content
      m1 <- docs1[[d]]$meta()
      m2 <- docs2[[d]]$meta()
      stopifnot(identical(m1, m2))
      stopifnot(!identical(t1, t2))
      print(paste0("Length of old content is ", length(t1)))
      print(paste0("Length of new content is ", length(t2)))
    })

    # Logit
    PreprocessCorpusBinTest$logs(className = className, methodName = "process", msg = paste("Successfully processed repair"))
    PreprocessCorpusBinTest$logs(className = className, methodName = "process", msg = paste("Successfully returned repaired object"))

    cat(paste0(test, " Completed: Success!\n"))

    return(cb)
  }


  test2 <- function(cb) {
    test <- "test2: Corpus: Repair with Custom by Decimal Code"
    cat(paste0("\n",test, " Commencing\n"))

    # Initialize parameters
    pattern = c(0,1,3,5,7,9,24,26, 127)
    replace = c(0x20, 0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20)
    subs = data.frame(pattern, replace)

    ct <- PreprocessCorpusBin$new(cb, "Caltech", substitutions = subs)$preprocess()$getResult()

    # Get news Corpus meta data
    c1 <- cb$exposeObject()
    c2 <- ct$exposeObject()
    stopifnot(c1$meta$name == 'Berkeley')
    stopifnot(c2$meta$name == 'Caltech')
    stopifnot(c1$meta$title == c2$meta$title)
    stopifnot(c1$meta$description == c2$meta$description)
    stopifnot(c1$meta$language == c2$meta$language)
    stopifnot(c1$meta$creator == c2$meta$creator)
    stopifnot(c1$meta$dateCreated == c2$meta$dateCreated)
    stopifnot(c1$meta$source == c2$meta$source)
    stopifnot(c1$meta$format == c2$meta$format)

    docs1 <- cb$getDocuments()
    docs2 <- ct$getDocuments()

    lapply(seq_along(docs1), function(d) {
      t1 <- docs1[[d]]$content
      t2 <- docs2[[d]]$content
      m1 <- docs1[[d]]$meta()
      m2 <- docs2[[d]]$meta()
      stopifnot(identical(m1, m2))
      stopifnot(!identical(t1, t2))
      print(paste0("Length of old content is ", length(t1)))
      print(paste0("Length of new content is ", length(t2)))
    })
    # Logit
    PreprocessCorpusBinTest$logs(className = className, methodName = "process", msg = paste("Successfully processed repair"))
    PreprocessCorpusBinTest$logs(className = className, methodName = "process", msg = paste("Successfully returned repaired object"))

    cat(paste0(test, " Completed: Success!\n"))

    return(cb)
  }

  testn <- function(news) {
    test <- "Testn: Corpus: Write File"
    cat(paste0("\n",test, " Commencing\n"))

    # Logit
    PreprocessCorpusBinTest$logs(className = className, methodName = "content", msg = paste("Successfully instantiated document with file"))

    cat(paste0(test, " Completed: Success!\n"))

    return(news)
  }


init()
cb <- test0()
cb <- test1(cb)
cb <- test2(cb)

}
className <- "PreprocessCorpusBin"

testPreprocessCorpusBin()
