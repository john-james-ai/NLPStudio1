testNLPStudio <- function() {

  init <- function() {

    devtools::load_all()
    base::unlink('./NLPStudio', recursive = TRUE)
    rm(list = ls(), envir = .GlobalEnv)
    source('./test/testFunctions/LogTest.R')

    # Initialize logger and NLPStudio objects
    nlpStudio <- NLPStudio$new()$getInstance()
    nlpStudioTests <<- LogTest$new()

    # Clean up
    if (exists("blue", envir = .GlobalEnv))  rm("blue", envir = .GlobalEnv)
    if (exists("beats", envir = .GlobalEnv)) rm("beats", envir = .GlobalEnv)
    return(nlpStudio)
  }

  # Test0: Confirm instantiation of nlpStudio
  test0 <- function(nlpStudio) {
    test <- "Test0: Instantiation"
    cat(paste(test, " Commencing\n"))

    # Test instantiation
    pipeline <- nlpStudio$exposeObject() #Return list with warning
    stopifnot(pipeline$name == "nlpStudio")
    stopifnot(pipeline$desc == "NLPStudio: Natural Language Processing Environment")
    stopifnot(length(pipeline$pipeline) == 0)
    stopifnot((Sys.time() - pipeline$created) < 1)
    stopifnot((Sys.time() - pipeline$modified) < 1)

    # Logit
    nlpStudioTests$logs(className = 'NLPStudio', methodName = "initialize", msg = "Successfully created nlpStudio")

    cat(paste(test, " Completed: Success!\n"))
    return(nlpStudio)
  }

  # Test1: Add pipeline
  test1 <- function(nlpStudio) {
    test <- "Test1: add pipeline"
    cat(paste(test, " Commencing\n"))

    # Add blue pipeline
    blue <- Pipeline$new(name = "blue", "Blue Studio")
    nlpStudio$addPipeline(blue)
    beats <- Pipeline$new(name = "beats", "Beats Studio")
    nlpStudio$addPipeline(beats)

    # Confirm object status
    pipeline <- nlpStudio$exposeObject() #Return list with warning
    stopifnot(pipeline$name == "nlpStudio")
    stopifnot(pipeline$desc == "NLPStudio: Natural Language Processing Environment")
    stopifnot(length(pipeline$pipeline) == 2)

    stopifnot(isTRUE(all.equal(pipeline$pipeline[[1]], blue)))
    stopifnot((Sys.time() - pipeline$created) < 1)
    stopifnot((Sys.time() - pipeline$modified) < 1)
    stopifnot(pipeline$created < pipeline$modified)
    stopifnot(dir.exists("./NLPStudio/pipelines/blue"))

    stopifnot(isTRUE(all.equal(pipeline$pipeline[[2]], beats)))
    stopifnot(pipeline$created < pipeline$modified)
    stopifnot(dir.exists("./NLPStudio/pipelines/beats"))

    # Logit
    nlpStudioTests$logs(className = 'NLPStudio', methodName = "addStudio",
                       msg = paste("Successfully added two pipeline to NLPStudio."))

    cat(paste(test, " Completed: Success!\n"))
    return(nlpStudio)
  }

  init()
  nlpStudio <- test0(nlpStudio)
  nlpStudio <- test1(nlpStudio)
  return(nlpStudio)

}

nlpStudio <- testNLPStudio()
