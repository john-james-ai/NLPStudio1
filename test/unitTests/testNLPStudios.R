testNLPStudios <- function() {

  init <- function() {

    devtools::load_all()
    base::unlink('./NLPStudios', recursive = TRUE)
    rm(list = ls(), envir = .GlobalEnv)
    source('./test/testFunctions/LogTest.R')

    # Initialize logger and NLPStudios objects
    nlpStudios <- NLPStudios$new()$getInstance()
    nlpStudiosTests <<- LogTest$new()

    # Clean up
    if (exists("blue", envir = .GlobalEnv))  rm("blue", envir = .GlobalEnv)
    if (exists("beats", envir = .GlobalEnv)) rm("beats", envir = .GlobalEnv)
    return(nlpStudios)
  }

  # Test0: Confirm instantiation of nlpStudios
  test0 <- function(nlpStudios) {
    test <- "Test0: Instantiation"
    cat(paste(test, " Commencing\n"))

    # Test instantiation
    studios <- nlpStudios$exposeObject() #Return list with warning
    stopifnot(studios$name == "nlpStudios")
    stopifnot(studios$desc == "NLPStudios: Natural Language Processing Environment")
    stopifnot(length(studios$studios) == 0)
    stopifnot((Sys.time() - studios$created) < 1)
    stopifnot((Sys.time() - studios$modified) < 1)

    # Logit
    nlpStudiosTests$logs(className = 'NLPStudios', methodName = "initialize", msg = "Successfully created nlpStudios")

    cat(paste(test, " Completed: Success!\n"))
    return(nlpStudios)
  }

  # Test1: Add studios
  test1 <- function(nlpStudios) {
    test <- "Test1: add studio"
    cat(paste(test, " Commencing\n"))

    # Add blue studio
    blue <- Studio$new(name = "blue", "Blue Studio")
    nlpStudios$addStudio(blue)
    beats <- Studio$new(name = "beats", "Beats Studio")
    nlpStudios$addStudio(beats)

    # Confirm object status
    studios <- nlpStudios$exposeObject() #Return list with warning
    stopifnot(studios$name == "nlpStudios")
    stopifnot(studios$desc == "NLPStudios: Natural Language Processing Environment")
    stopifnot(length(studios$studios) == 2)

    stopifnot(isTRUE(all.equal(studios$studios[[1]], blue)))
    stopifnot((Sys.time() - studios$created) < 1)
    stopifnot((Sys.time() - studios$modified) < 1)
    stopifnot(studios$created < studios$modified)
    stopifnot(dir.exists("./NLPStudios/studios/blue"))

    stopifnot(isTRUE(all.equal(studios$studios[[2]], beats)))
    stopifnot(studios$created < studios$modified)
    stopifnot(dir.exists("./NLPStudios/studios/beats"))

    # Logit
    nlpStudiosTests$logs(className = 'NLPStudios', methodName = "addStudio",
                       msg = paste("Successfully added two studios to NLPStudios."))

    cat(paste(test, " Completed: Success!\n"))
    return(nlpStudios)
  }

  init()
  nlpStudios <- test0(nlpStudios)
  nlpStudios <- test1(nlpStudios)
  return(nlpStudios)

}

nlpStudios <- testNLPStudios()
