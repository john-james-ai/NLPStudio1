testNLPStudio <- function() {

  init <- function() {

<<<<<<< HEAD
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
=======
    # Clean up
    if (exists("blue", envir = .GlobalEnv)) rm("blue", envir = .GlobalEnv)
    if (exists("beats", envir = .GlobalEnv)) rm("beats", envir = .GlobalEnv)

  }

  # Test0: Confirm instantiation of nlpStudio
  test0 <- function() {
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
    test <- "Test0: Instantiation"
    cat(paste(test, " Commencing\n"))

    # Test instantiation
<<<<<<< HEAD
    studio <- nlpStudio$exposeObject() #Return list with warning
=======
    studio <<- nlpStudio$exposeObject() #Return list with warning
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
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
<<<<<<< HEAD
    return(nlpStudio)
  }

  # Test1: Add labs
  test1 <- function(nlpStudio) {
=======
  }


  # Test1: Add lab
  test1 <- function() {
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
    test <- "Test1: add lab"
    cat(paste(test, " Commencing\n"))

    # Add blue lab
    blue <- Lab$new(name = "blue", "Blue Lab")
<<<<<<< HEAD
    nlpStudio$addLab(blue)
    beats <- Lab$new(name = "beats", "Beats Lab")
    nlpStudio$addLab(beats)

    # Confirm object status
    studio <- nlpStudio$exposeObject() #Return list with warning
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Environment")
    stopifnot(length(studio$labs) == 2)

=======
    nlpStudio <<- NLPStudio$new()$getInstance()
    nlpStudio <<- nlpStudio$addChild(blue)

    # Confirm object status
    studio <<- nlpStudio$exposeObject() #Return list with warning
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Environment")
    stopifnot(length(studio$labs) == 1)
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
    stopifnot(isTRUE(all.equal(studio$labs[[1]], blue)))
    stopifnot((Sys.time() - studio$created) < 1)
    stopifnot((Sys.time() - studio$modified) < 1)
    stopifnot(studio$created < studio$modified)
<<<<<<< HEAD
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
=======
    stopifnot(dir.exists("./NLPStudio/labs/blue/data"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/data/external"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/data/raw"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/data/munge"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/data/processed"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/logs"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/reports"))

    # Logit
    nlpStudioTests$logs(className = 'NLPStudio', methodName = "addChild",
                       msg = paste(blue$getName(), "lab successfully added to NLPStudio."))

    cat(paste(test, " Completed: Success!\n"))
  }


  # Test2: Add 2nd lab
  test2 <- function() {
    test <- "Test2: add 2nd lab"
    cat(paste(test, " Commencing\n"))

    # Add blue lab
    beats <- Lab$new(name = "beats", "Beats Lab")
    nlpStudio <<- nlpStudio$addChild(beats)

    # Confirm object status
    studio <<- nlpStudio$exposeObject() #Return list with warning
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Environment")
    stopifnot(length(studio$labs) == 2)
    stopifnot(isTRUE(all.equal(studio$labs[[2]], beats)))
    stopifnot(studio$created < studio$modified)
    stopifnot(dir.exists("./NLPStudio/labs/beats/data"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/data/external"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/data/raw"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/data/munge"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/data/processed"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/logs"))
    stopifnot(dir.exists("./NLPStudio/labs/beats/reports"))
    # Logit
    nlpStudioTests$logs(className = 'NLPStudio', methodName = "addChild",
                        msg = paste(blue$getName(), "lab successfully added to NLPStudio."))

    cat(paste(test, " Completed: Success!\n"))
  }

  # Test3: Set lab current
  test3 <- function() {
    test <- "Test3: Set lab current"
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
    cat(paste(test, " Commencing\n"))

    # Set lab current
    #nlpStudio$currentLab <- badsa  # should error
    nlpStudio$currentLab <- blue
<<<<<<< HEAD

    # Confirm object status
    stopifnot(isTRUE(all.equal(nlpStudio$currentLab, blue)))

    # Logit
    nlpStudioTests$logs(className = 'NLPStudio', methodName = "currentLab",
                        msg = paste('Current lab set to', blue$getName()))

    cat(paste(test, " Completed: Success!\n"))
    return(nlpStudio)
=======
    studio <- nlpStudio$exposeObject()

    # Confirm object status
    stopifnot(isTRUE(all.equal(nlpStudio$currentLab, blue)))
    stopifnot(length(studio$labs) == 2)
    stopifnot(isTRUE(all.equal(studio$labs[[1]], blue)))
    stopifnot(isTRUE(all.equal(studio$labs[[2]], beats)))
    stopifnot(studio$created < studio$modified)
    stopifnot(dir.exists("./NLPStudio/labs/beats/reports"))
    currentLab <- nlpStudio$currentLab

    # Logit
    nlpStudioTests$logs(className = 'NLPStudio', methodName = "currentLab",
                        msg = paste('Current lab set to', currentLab$getName()))

    cat(paste(test, " Completed: Success!\n"))
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
  }


  init()
<<<<<<< HEAD
  nlpStudio <- test0(nlpStudio)
  nlpStudio <- test1(nlpStudio)
  nlpStudio <- test2(nlpStudio)
  return(nlpStudio)

}

nlpStudio <- testNLPStudio()
=======
  test0()
  test1()
  test2()
  test3()

}

devtools::load_all()
c <- Constants$new()
dirs <- c$getStudioPaths()
lapply(dirs, function(d) {base::unlink(d, recursive = TRUE)})

# Initialize logger and NLPStudio objects
nlpStudio <<- NLPStudio$new()$getInstance()
nlpStudioTests <<- LogTest$new()
testNLPStudio()
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
