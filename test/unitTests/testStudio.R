testStudio <- function(nlpStudios) {
  init <- function(nlpStudios) {
    # Clean up
    if (exists("predictifyR", envir = .GlobalEnv)) rm(list = ls(envir = .GlobalEnv)[grep("predictifyR", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (!is.null(nlpStudios$getStudios()[['predictifyR']])) nlpStudios$removeStudio(predictifyR)
    studioTests <<- LogTest$new()
    return(nlpStudios)
  }

  # Test 0: Confirm instantiation of studio
  test0 <- function(nlpStudios) {
    test <- "test0: Studio Instantiation"
    cat(paste0("\n",test, " Commencing\n"))

    # Test Instantiation
    # Studio$new() # should fail, name is required: Success
    predictifyR <- Studio$new(name = "predictifyR", "PredictifyR Studio")
    nlpStudios <- nlpStudios$addStudio(predictifyR)

    # Confirm instantiation
    l <- predictifyR$exposeObject()
    stopifnot("Studio" %in% class(predictifyR))
    stopifnot(l$name == "predictifyR")
    stopifnot(l$desc == "PredictifyR Studio")
    stopifnot(l$path == "./NLPStudios/studios/predictifyR")
    stopifnot(isTRUE(all.equal(l$parent$getName(), nlpStudios$getName())))
    stopifnot((Sys.time() - l$created) < 1)
    stopifnot((Sys.time() - l$modified) < 1)
    stopifnot(dir.exists("./NLPStudios/studios/predictifyR"))

    # Logit
    studioTests$logs(className = className, methodName = "initiate", msg = paste("Successfully created studio:", l$name))
    cat(paste0(test, " Completed: Success!\n"))
    return(predictifyR)
  }


  nlpStudios <- init(nlpStudios)
  predictifyR <- test0(nlpStudios)
}

className <- "Studio"

source('./test/unitTests/testNLPStudios.R')
predictifyR <- testStudio(nlpStudios)
