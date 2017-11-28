testNLPStudio <- function() {

  init <- function() {

    # Clean up
    if (exists("Beats", envir = .GlobalEnv)) rm("Beats", envir = .GlobalEnv)
    if (exists("Bart", envir = .GlobalEnv))rm("Bart", envir = .GlobalEnv)

  }

  # Test 0: Confirm instantiation of nlpStudio
  test0 <- function() {
    test <- "test0: Instantiation"
    cat(paste(test, " Commencing\n"))

    # Test instantiation
    studio <<- nlpStudio$exposeObject() #Return list with warning
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Environment")
    stopifnot(length(studio$labs) == 0)
    stopifnot((Sys.time() - studio$created) < 1)
    stopifnot((Sys.time() - studio$modified) < 1)
    stopifnot(studio$created == studio$modified)

    # Logit
    logTest$log(className = 'NLPStudio', methodName = "initialize", msg = "Successfully created nlpStudio")

    cat(paste(test, " Completed: Success!\n"))
  }


  init()
  test0()

}

devtools::load_all()
c <- Constants$new()
dirs <- c$getStudioPaths()
lapply(dirs, function(d) {base::unlink(d, recursive = TRUE)})

# Initialize logger and NLPStudio objects
nlpStudio <<- NLPStudio$new()$getInstance()
logTest <<- LogTest$new()
testNLPStudio()
