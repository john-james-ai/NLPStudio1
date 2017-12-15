testFile <- function() {

  init <- function() {
    source('./test/testFunctions/LogTest.R')
    if (exists("blogs", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("blogs", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("news", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("news", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("twitter", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("twitter", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("blogsTxt", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("blogsTxt", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("blogsRdata", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("blogsRdata", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("blogsRds", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("blogsRds", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("blogsCsv", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("blogsCsv", ls(envir = .GlobalEnv))], envir = .GlobalEnv)

    FileTest <<- LogTest$new()
  }

  test0 <- function() {
    test <- "test0: File: Instantiation document/ no File"
    cat(paste0("\n",test, " Commencing\n"))

    # Validation
    # blogs <- Document$new()# should fail, no name

    # Instantiate
    blogs <- Document$new(name = 'blogs')
    d <- blogs$exposeObject()
    stopifnot(d$className == 'Document')
    stopifnot(d$name == 'blogs')
    stopifnot(d$desc == 'blogs')
    stopifnot(length(d$parent) == 0)
    stopifnot(length(d$path) == 0)
    stopifnot(length(d$content) == 0)
    stopifnot((Sys.time() -  d$created) < 1)
    stopifnot((Sys.time() -  d$modified) < 1)
    stopifnot((Sys.time() -  d$accessed) < 1)

    stopifnot(blogs$getClassName() == "Document")
    stopifnot(blogs$getName() == "blogs")
    stopifnot(is.null(blogs$getPath()))
    stopifnot(is.null(blogs$getContent()))

    # Logit
    FileTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized document"))
    FileTest$logs(className = className, methodName = "getClassName", msg = paste("Successfully obtained class name"))
    FileTest$logs(className = className, methodName = "getName", msg = paste("Successfully obtained name"))
    FileTest$logs(className = className, methodName = "getPath", msg = paste("Successfully obtained path"))
    cat(paste0(test, " Completed: Success!\n"))

    return(blogs)
  }

  test1 <- function(blogs) {
    test <- "test1: File: Create Text Documents"
    cat(paste0("\n",test, " Commencing\n"))

    init()

    # Instantiate original and copies
    blogs <- Document$new(name = 'blogs', './test/testData/en_US.blogs.txt')
    blogsTxt <- Document$new(name = 'blogsTxt', './test/testData/output/blogs.txt')
    blogsRdata <- Document$new(name = 'blogsRdata', './test/testData/output/blogs.rdata')
    blogsRds <- Document$new(name = 'blogsRds', './test/testData/output/blogs.rds')
    blogsCsv <- Document$new(name = 'blogsCsv', './test/testData/output/blogs.csv')

    # Load copies
    content <- blogs$getContent()
    blogsTxt$addContent(content)
    blogsRdata$addContent(content)
    blogsRds$addContent(content)
    blogsCsv$addContent(content)

    # Write new documents
    blogsTxt$write()
    blogsRdata$write()
    blogsRds$write()
    blogsCsv$write()

    # Test
    stopifnot(file.exists('./test/testData/output/blogs.txt'))
    stopifnot(file.exists('./test/testData/output/blogs.rdata'))
    stopifnot(file.exists('./test/testData/output/blogs.rds'))
    stopifnot(file.exists('./test/testData/output/blogs.csv'))

    blogTxtContent <- blogsTxt$read()
    blogRdataContent <- blogsRdata$read()
    blogRdsContent <- blogsRds$read()
    blogCsvContent <- blogsCsv$read()

    stopifnot(length(blogTxtContent) > 1000)
    stopifnot(length(blogRdataContent) > 1000)
    stopifnot(length(blogRdsContent) > 1000)
    stopifnot(nrow(blogCsvContent) > 1000)

    # Logit
    FileTest$logs(className = className, methodName = "read", msg = paste("Successfully read txt document "))
    FileTest$logs(className = className, methodName = "read", msg = paste("Successfully read rdata document "))
    FileTest$logs(className = className, methodName = "read", msg = paste("Successfully read rds document "))
    FileTest$logs(className = className, methodName = "read", msg = paste("Successfully read csv document "))

    FileTest$logs(className = className, methodName = "addContent", msg = paste("Successfully add content to txt document "))
    FileTest$logs(className = className, methodName = "addContent", msg = paste("Successfully add content to rdata document "))
    FileTest$logs(className = className, methodName = "addContent", msg = paste("Successfully add content to rds document "))
    FileTest$logs(className = className, methodName = "addContent", msg = paste("Successfully add content to csv document "))

    FileTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote to txt document "))
    FileTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote to rdata document "))
    FileTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote to rds document "))
    FileTest$logs(className = className, methodName = "write", msg = paste("Successfully wrote to csv document "))

    cat(paste0(test, " Completed: Success!\n"))

    return(blogCsvContent)
  }


init()
blogs <<- test0()
blogCsvContent <<- test1(blogs)
}
className <- "File"
#source('./test/unitTests/testCorpusBuilder.R')
testFile()
