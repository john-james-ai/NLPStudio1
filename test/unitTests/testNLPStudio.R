testNLPStudio <- function() {

  init <- function() {

    devtools::load_all()
    c <- Constants$new()
    dirs <- c$getStudioPaths()
    lapply(dirs, function(d) {base::unlink(d, recursive = TRUE)})

    # Initialize logger and NLPStudio objects
    rm("nlpStudio", envir = .GlobalEnv)
    nlpStudio <- NLPStudio$new()$getInstance()
    nlpStudioTests <- LogTest$new()

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
    studio <- nlpStudio$exposeObject() #Return list with warning
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Environment")
    stopifnot(length(studio$labs) == 0)
    stopifnot((Sys.time() - studio$created) < 1)
    stopifnot((Sys.time() - studio$modified) < 1)
    stopifnot(dir.exists("./NLPStudio/config"))
    stopifnot(dir.exists("./NLPStudio/labs"))
    stopifnot(dir.exists("./NLPStudio/logs"))

    # Logit
    nlpStudioTests$logs(className = 'NLPStudio', methodName = "initialize", msg = "Successfully created nlpStudio")

    cat(paste(test, " Completed: Success!\n"))
    return(nlpStudio)
  }

  # Test1: Add labs
  test1 <- function(nlpStudio) {
    test <- "Test1: add lab"
    cat(paste(test, " Commencing\n"))

    # Add blue lab
    blue <- Lab$new(name = "blue", "Blue Lab")
    nlpStudio$addLab(blue)
    beats <- Lab$new(name = "beats", "Beats Lab")
    nlpStudio$addLab(beats)

    # Confirm object status
    studio <- nlpStudio$exposeObject() #Return list with warning
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Environment")
    stopifnot(length(studio$labs) == 2)

    stopifnot(isTRUE(all.equal(studio$labs[[1]], blue)))
    stopifnot((Sys.time() - studio$created) < 1)
    stopifnot((Sys.time() - studio$modified) < 1)
    stopifnot(studio$created < studio$modified)
    stopifnot(dir.exists("./NLPStudio/labs/blue/corpora"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/archives"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/logs"))

    stopifnot(isTRUE(all.equal(studio$labs[[2]], beats)))
    stopifnot(studio$created < studio$modified)
    stopifnot(dir.exists("./NLPStudio/labs/beats/corpora"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/archives"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/logs"))

    # Logit
    nlpStudioTests$logs(className = 'NLPStudio', methodName = "addLab",
                       msg = paste("Successfully added two labs to NLPStudio."))

    cat(paste(test, " Completed: Success!\n"))
    return(nlpStudio)
  }

  # Test2: Set lab current
  test2 <- function(nlpStudio) {
    test <- "Test2: Set lab current"
    cat(paste(test, " Commencing\n"))

    # Set lab current
    #nlpStudio$currentLab <- badsa  # should error
    nlpStudio$currentLab <- blue

    # Confirm object status
    stopifnot(isTRUE(all.equal(nlpStudio$currentLab, blue)))

    # Logit
    nlpStudioTests$logs(className = 'NLPStudio', methodName = "currentLab",
                        msg = paste('Current lab set to', blue$getName()))

    cat(paste(test, " Completed: Success!\n"))
    return(nlpStudio)
  }


  init()
  nlpStudio <- test0(nlpStudio)
  nlpStudio <- test1(nlpStudio)
  nlpStudio <- test2(nlpStudio)
  return(nlpStudio)

}

nlpStudio <- testNLPStudio()
