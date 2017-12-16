testLab <- function() {
  init <- function() {

    # Clean up
    if (exists("predictifyR", envir = .GlobalEnv)) rm(list = ls(envir = .GlobalEnv)[grep("predictifyR", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    return(nlpStudio)
  }

  # Test 0: Confirm instantiation of lab
  test0 <- function() {
    test <- "test0: Lab Instantiation"
    cat(paste0("\n",test, " Commencing\n"))

    # Test Instantiation
    # Lab$new() # should fail, name is required: Success
    predictifyR <- Lab$new(name = "predictifyR", "PredictifyR Lab")

    # Confirm instantiation
    l <- predictifyR$exposeObject()
    stopifnot("Lab" %in% class(predictifyR))
    stopifnot(l$name == "predictifyR")
    stopifnot(l$desc == "PredictifyR Lab")
    stopifnot(l$path == "./NLPStudio/labs/predictifyR")
    stopifnot(isTRUE(all.equal(l$parent, nlpStudio)))
    stopifnot((Sys.time() - l$created) < 1)
    stopifnot((Sys.time() - l$modified) < 1)
    stopifnot(dir.exists("./NLPStudio/labs/predictifyR"))

    # Logit
    labTests$logs(className = className, methodName = "initiate", msg = paste("Successfully created lab:", l$name))
    cat(paste0(test, " Completed: Success!\n"))
    return(predictifyR)
  }


  init()
  test0()
}

className <- "Lab"

source('./test/unitTests/testNLPStudio.R')
predictifyR <- testLab()
