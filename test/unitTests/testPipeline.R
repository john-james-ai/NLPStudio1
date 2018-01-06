testPipeline <- function() {
  init <- function() {
    name <<- "pipeline"
    path <<- "./test/testData/pipeline"
    unlink(path, recursive = TRUE)
    if (exists("pipeline", envir = .GlobalEnv)) rm(list = ls(envir = .GlobalEnv)[grep("pipeline", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    pipelineTests <<- LogTest$new()
  }

  # Test 0: Confirm instantiation of pipeline
  test0 <- function() {
    test <- "test0: Pipeline Instantiation"
    cat(paste0("\n",test, " Commencing\n"))

    # Create pipeline object
    pipeline <- Pipeline$new(name, path)

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
