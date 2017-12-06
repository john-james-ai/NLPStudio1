testKorpus <- function() {
  init <- function() {

    # Clean up
    if (exists("stanford", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("stanford", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("oxford", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("oxford", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
  }

  # Test 0: Confirm instantiation of lab
  test0 <- function() {
    test <- "test0: Korpus Instantiation 1"
    cat(paste0("\n",test, " Commencing\n"))

    # Test Instantiation
    #Korpus$new() # should fail, name is required: Success
    Korpus$new(name = "stanford") # should succeed

    # Confirm instantiation
    k <- stanford$exposeObject()
    stopifnot("Korpus" %in% class(stanford))
    stopifnot(k$name == "stanford")
    stopifnot(k$desc == "stanford corpus")
    stopifnot(k$path == "./NLPStudio/corpora/stanford")
    stopifnot((Sys.time() - k$created) < 1)
    stopifnot((Sys.time() - k$modified) < 1)
    stopifnot(k$dirs$data == 'data')
    stopifnot(k$dirs$external == 'data/external')
    stopifnot(k$dirs$raw == 'data/raw')
    stopifnot(k$dirs$processed == 'data/sets')
    stopifnot(dir.exists(file.path(k$path)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$data)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$external)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$raw)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$sets)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$reports)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$logs)))

    # Logit
    korpusTests$logs(className = className, methodName = "initiate", msg = paste("Successfully created corpus:", k$name))
    cat(paste0(test, " Completed: Success!\n"))

    return(stanford)
  }

  test1 <- function() {
    test <- "test1: Korpus Instantiation 2"
    cat(paste0("\n",test, " Commencing\n"))
    Korpus$new(name = "oxford")

    # Confirm instantiation
    k <- oxford$exposeObject()
    stopifnot("Korpus" %in% class(oxford))
    stopifnot(k$name == "oxford")
    stopifnot(k$desc == "oxford corpus")
    stopifnot(k$path == "./NLPStudio/corpora/oxford")
    stopifnot((Sys.time() - k$created) < 1)
    stopifnot((Sys.time() - k$modified) < 1)
    stopifnot(k$dirs$data == 'data')
    stopifnot(k$dirs$external == 'data/external')
    stopifnot(k$dirs$raw == 'data/raw')
    stopifnot(k$dirs$sets == 'data/sets')
    stopifnot(k$dirs$reports == 'reports')
    stopifnot(dir.exists(file.path(k$path)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$data)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$external)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$raw)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$sets)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$reports)))

    # Logit
    korpusTests$logs(className = className, methodName = "initiate", msg = paste("Successfully created corpus:", k$name))
    cat(paste0(test, " Completed: Success!\n"))

    return(oxford)
  }

  test2 <- function(stanford) {
    test <- "test2: Add Corpus to Lab"
    cat(paste0("\n",test, " Commencing\n"))

    # Add corpora to labs
    blue <- blue$addKorpus(stanford)
    k <- stanford$exposeObject()

    # Confirm corpus added
    l <- blue$exposeObject()
    stopifnot(isTRUE(all.equal(l$korpora[[1]], stanford)))
    stopifnot(dir.exists("./NLPStudio/labs/blue/stanford"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/stanford/data"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/stanford/data/external"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/stanford/data/raw"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/stanford/data/sets"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/stanford/reports"))

    stopifnot(!dir.exists("./NLPStudio/stanford"))
    stopifnot(!dir.exists("./NLPStudio/stanford/data"))
    stopifnot(!dir.exists("./NLPStudio/stanford/data/external"))
    stopifnot(!dir.exists("./NLPStudio/stanford/data/raw"))
    stopifnot(!dir.exists("./NLPStudio/stanford/data/sets"))
    stopifnot(!dir.exists("./NLPStudio/stanford/reports"))

    # Logit
    korpusTests$logs(className = className, methodName = "addKorpus", msg = paste("Successfully added corpus:", k$name, "to lab", l$name))


    # Logit
    #korpusTests$logs(className = className, methodName = "addKorpus", msg = paste("Successfully added corpus:", k$name, "to lab", l$name))
    cat(paste0(test, " Completed: Success!\n"))

    return(stanford)
  }

  test3 <- function(oxford) {
    test <- "test3: Add Corpus to 2nd Lab"
    cat(paste0("\n",test, " Commencing\n"))
    beats <- beats$addKorpus(oxford)
    k <- oxford$exposeObject()

    # Confirm corpus added
    l <- beats$exposeObject()
    stopifnot(isTRUE(all.equal(l$korpora[[1]], oxford)))
    stopifnot(dir.exists("./NLPStudio/labs/beats/oxford"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/oxford/data"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/oxford/data/external"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/oxford/data/raw"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/oxford/data/sets"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/oxford/reports"))

    stopifnot(!dir.exists("./NLPStudio/oxford"))
    stopifnot(!dir.exists("./NLPStudio/oxford/data"))
    stopifnot(!dir.exists("./NLPStudio/oxford/data/external"))
    stopifnot(!dir.exists("./NLPStudio/oxford/data/raw"))
    stopifnot(!dir.exists("./NLPStudio/oxford/data/sets"))
    stopifnot(!dir.exists("./NLPStudio/oxford/reports"))


    # Logit
    #korpusTests$logs(className = className, methodName = "addKorpus", msg = paste("Successfully added corpus:", k$name, "to lab", l$name))
    cat(paste0(test, " Completed: Success!\n"))

    return(oxford)
  }

  test4 <- function(stanford) {
    test <- "test4: getCorpus"
    cat(paste0("\n",test, " Commencing\n"))
    print("*************************************")

    url = 'http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
    files <- c('final/en_US/en_US.blogs.txt', 'final/en_US/en_US.news.txt', 'final/en_US/en_US.twitter.txt')
    status <- stanford$getKorpus(url = url, files = files, listFiles = FALSE)

    # Logit
    #    korpusTests$logs(className = className, methodName = "addKorpus", msg = paste("Successfully added corpus:", k$name, "to lab", l$name))
    cat(paste0(test, " Completed: Success!\n"))

    return(status)
  }

  init()
  stanford <- test0()
  oxford   <- test1()
  stanford <- test2(stanford)
  oxford   <- test3(oxford)
  stanford <- test4(stanford)
}

className <- "Korpus"

source('./test/unitTests/testLab.R')
testKorpus()
