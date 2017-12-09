testLab <- function(nlpStudio) {
  init <- function(nlpStudio) {
    # Clean up
    if (exists("predictifyR", envir = .GlobalEnv)) rm(list = ls(envir = .GlobalEnv)[grep("predictifyR", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (!is.null(nlpStudio$getLabs()[['predictifyR']])) nlpStudio$removeLab(predictifyR)
    labTests <<- LogTest$new()
    return(nlpStudio)
  }

  # Test 0: Confirm instantiation of lab
  test0 <- function(nlpStudio) {
    test <- "test0: Lab Instantiation"
    cat(paste0("\n",test, " Commencing\n"))

    # Test Instantiation
    # Lab$new() # should fail, name is required: Success
    predictifyR <- Lab$new(name = "predictifyR", "PredictifyR Lab")
    nlpStudio <- nlpStudio$addLab(predictifyR)

    # Confirm instantiation
    l <- predictifyR$exposeObject()
    stopifnot("Lab" %in% class(predictifyR))
    stopifnot(l$name == "predictifyR")
    stopifnot(l$desc == "PredictifyR Lab")
    stopifnot(l$path == "./NLPStudio/labs/predictifyR")
    stopifnot(isTRUE(all.equal(l$parent$getName(), nlpStudio$getName())))
    stopifnot((Sys.time() - l$created) < 1)
    stopifnot((Sys.time() - l$modified) < 1)
    stopifnot(dir.exists("./NLPStudio/labs/predictifyR"))

    # Logit
    labTests$logs(className = className, methodName = "initiate", msg = paste("Successfully created lab:", l$name))
    cat(paste0(test, " Completed: Success!\n"))
    return(predictifyR)
  }


  nlpStudio <- init(nlpStudio)
  predictifyR <- test0(nlpStudio)
}

className <- "Lab"

source('./test/unitTests/testNLPStudio.R')
predictifyR <- testLab(nlpStudio)
