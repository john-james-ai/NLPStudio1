testKorpus <- function() {
  init <- function() {

    # Clean up
    if (exists("stanford", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("stanford", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("oxford", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("oxford", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
  }

  # Test 0: Confirm instantiation of lab
  test0 <- function() {
    test <- "test0: Korpus Instantiation"
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
    stopifnot(k$dirs$processed == 'data/preprocessed')
    stopifnot(k$dirs$processed == 'data/sets')
    stopifnot(dir.exists(file.path(k$path)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$data)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$external)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$raw)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$preprocessed)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$sets)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$reports)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$logs)))

    # Logit
    korpusTests$logs(className = className, methodName = "initiate", msg = paste("Successfully created corpus:", k$name))

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
    stopifnot(k$dirs$processed == 'data/preprocessed')
    stopifnot(k$dirs$processed == 'data/sets')
    stopifnot(dir.exists(file.path(k$path)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$data)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$external)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$raw)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$preprocessed)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$sets)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$reports)))
    stopifnot(dir.exists(file.path(k$path, k$dirs$logs)))

    # Logit
    korpusTests$logs(className = className, methodName = "initiate", msg = paste("Successfully created corpus:", k$name))
    cat(paste0(test, " Completed: Success!\n"))
  }

  # Test 0: Add korpus to lab
  test1 <- function() {
    test <- "test1: Add Corpus to Lab"
    cat(paste0("\n",test, " Commencing\n"))

    # Add corpora to labs
    blue <- blue$addKorpus(stanford)
    k <- stanford$exposeObject()

    # Confirm corpus added
    l <- blue$exposeObject()
    stopifnot(isTRUE(all.equal(l$korpora[[1]], stanford)))
    stopifnot(dir.exists("./NLPStudio/labs/blue/corpora/stanford"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/corpora/stanford/data"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/corpora/stanford/data/external"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/corpora/stanford/data/raw"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/corpora/stanford/data/sets"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/corpora/stanford/data/preprocessed"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/corpora/stanford/logs"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/corpora/stanford/reports"))

    stopifnot(!dir.exists("./NLPStudio/corpora/stanford"))
    stopifnot(!dir.exists("./NLPStudio/corpora/stanford/data"))
    stopifnot(!dir.exists("./NLPStudio/corpora/stanford/data/external"))
    stopifnot(!dir.exists("./NLPStudio/corpora/stanford/data/raw"))
    stopifnot(!dir.exists("./NLPStudio/corpora/stanford/data/sets"))
    stopifnot(!dir.exists("./NLPStudio/corpora/stanford/data/preprocessed"))
    stopifnot(!dir.exists("./NLPStudio/corpora/stanford/logs"))
    stopifnot(!dir.exists("./NLPStudio/corpora/stanford/reports"))

    # Logit
    korpusTests$logs(className = className, methodName = "addKorpus", msg = paste("Successfully added corpus:", k$name, "to lab", l$name))

    beats <- beats$addKorpus(oxford)
    k <- oxford$exposeObject()

    # Confirm corpus added
    l <- beats$exposeObject()
    stopifnot(isTRUE(all.equal(l$korpora[[1]], oxford)))
    stopifnot(dir.exists("./NLPStudio/labs/beats/corpora/oxford"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/corpora/oxford/data"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/corpora/oxford/data/external"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/corpora/oxford/data/raw"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/corpora/oxford/data/sets"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/corpora/oxford/data/preprocessed"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/corpora/oxford/logs"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/corpora/oxford/reports"))

    stopifnot(!dir.exists("./NLPStudio/corpora/oxford"))
    stopifnot(!dir.exists("./NLPStudio/corpora/oxford/data"))
    stopifnot(!dir.exists("./NLPStudio/corpora/oxford/data/external"))
    stopifnot(!dir.exists("./NLPStudio/corpora/oxford/data/raw"))
    stopifnot(!dir.exists("./NLPStudio/corpora/oxford/data/sets"))
    stopifnot(!dir.exists("./NLPStudio/corpora/oxford/data/preprocessed"))
    stopifnot(!dir.exists("./NLPStudio/corpora/oxford/logs"))
    stopifnot(!dir.exists("./NLPStudio/corpora/oxford/reports"))


    # Logit
    #korpusTests$logs(className = className, methodName = "addKorpus", msg = paste("Successfully added corpus:", k$name, "to lab", l$name))
    cat(paste0(test, " Completed: Success!\n"))
  }


  init()
  test0()
  test1()
}

className <- "Korpus"

source('./test/unitTests/testLab.R')
testKorpus()
