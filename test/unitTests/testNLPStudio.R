testNLPStudio <- function() {

  init <- function() {

    # Clean up
    if (exists("blue", envir = .GlobalEnv)) rm("blue", envir = .GlobalEnv)
    if (exists("beats", envir = .GlobalEnv)) rm("beats", envir = .GlobalEnv)

  }

  # Test0: Confirm instantiation of nlpStudio
  test0 <- function() {
    test <- "Test0: Instantiation"
    cat(paste(test, " Commencing\n"))

    # Test instantiation
    studio <<- nlpStudio$exposeObject() #Return list with warning
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
  }


  # Test1: Add lab
  test1 <- function() {
    test <- "Test1: add lab"
    cat(paste(test, " Commencing\n"))

    # Add blue lab
    blue <- Lab$new(name = "blue", "Blue Lab")
    nlpStudio <<- NLPStudio$new()$getInstance()
    nlpStudio <<- nlpStudio$addChild(blue)

    # Confirm object status
    studio <<- nlpStudio$exposeObject() #Return list with warning
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Environment")
    stopifnot(length(studio$labs) == 1)
    stopifnot(isTRUE(all.equal(studio$labs[[1]], blue)))
    stopifnot((Sys.time() - studio$created) < 1)
    stopifnot((Sys.time() - studio$modified) < 1)
    stopifnot(studio$created < studio$modified)
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
    cat(paste(test, " Commencing\n"))

    # Set lab current
    #nlpStudio$currentLab <- badsa  # should error
    nlpStudio$currentLab <- blue
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
  }


  init()
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
