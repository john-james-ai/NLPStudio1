testPipeline <- function(nlpStudio) {
  init <- function(nlpStudio) {
    # Clean up
    if (exists("predictifyR", envir = .GlobalEnv)) rm(list = ls(envir = .GlobalEnv)[grep("predictifyR", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (!is.null(nlpStudio$getPipelines()[['predictifyR']])) nlpStudio$removePipeline(predictifyR)
    pipelineTests <<- LogTest$new()
    return(nlpStudio)
  }

  # Test 0: Confirm instantiation of pipeline
  test0 <- function(nlpStudio) {
    test <- "test0: Pipeline Instantiation"
    cat(paste0("\n",test, " Commencing\n"))

    # Test Instantiation
    # Pipeline$new() # should fail, name is required: Success
    predictifyR <- Pipeline$new(name = "predictifyR", "PredictifyR Pipeline")
    nlpStudio <- nlpStudio$addPipeline(predictifyR)

    # Confirm instantiation
    l <- predictifyR$exposeObject()
    stopifnot("Pipeline" %in% class(predictifyR))
    stopifnot(l$name == "predictifyR")
    stopifnot(l$desc == "PredictifyR Pipeline")
    stopifnot(l$path == "./NLPStudio/pipelines/predictifyR")
    stopifnot(isTRUE(all.equal(l$parent$getName(), nlpStudio$getName())))
    stopifnot((Sys.time() - l$created) < 1)
    stopifnot((Sys.time() - l$modified) < 1)
    stopifnot(dir.exists("./NLPStudio/pipelines/predictifyR"))

    # Logit
    pipelineTests$logs(className = className, methodName = "initiate", msg = paste("Successfully created pipeline:", l$name))
    cat(paste0(test, " Completed: Success!\n"))
    return(predictifyR)
  }


  nlpStudio <- init(nlpStudio)
  predictifyR <- test0(nlpStudio)
}

className <- "Pipeline"

source('./test/unitTests/testNLPStudio.R')
predictifyR <- testPipeline(nlpStudio)
