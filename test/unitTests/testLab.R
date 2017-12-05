testLab <- function() {
  init <- function() {

    # Clean up
<<<<<<< HEAD
    if (exists("predictifyR", envir = .GlobalEnv)) rm(list = ls(envir = .GlobalEnv)[grep("predictifyR", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    return(nlpStudio)
=======
    if (exists("blue", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("blue", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("brown", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("brown", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("oxford", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("oxford", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
  }

  # Test 0: Confirm instantiation of lab
  test0 <- function() {
    test <- "test0: Lab Instantiation"
    cat(paste0("\n",test, " Commencing\n"))

    # Test Instantiation
    # Lab$new() # should fail, name is required: Success
<<<<<<< HEAD
    predictifyR <- Lab$new(name = "predictifyR", "PredictifyR Lab")

    # Confirm instantiation
    l <- predictifyR$exposeObject()
    stopifnot("Lab" %in% class(predictifyR))
    stopifnot(l$name == "predictifyR")
    stopifnot(l$desc == "PredictifyR Lab")
    stopifnot(l$path == "./NLPStudio/labs/predictifyR")
    stopifnot(isTRUE(all.equal(l$parent, nlpStudio)))
    stopifnot((Sys.time() - l$created) < 1)
    stopifnot((Sys.time() - l$modified) < 1)
    stopifnot(dir.exists("./NLPStudio/labs/predictifyR"))

    # Logit
    labTests$logs(className = className, methodName = "initiate", msg = paste("Successfully created lab:", l$name))
    cat(paste0(test, " Completed: Success!\n"))
    return(predictifyR)
=======
    blue <- Lab$new(name = "blue", "Blue Lab")
    #blue <- Lab$new(name = "blue", "Blue Lab") # Error lab exists and directory already exists: success


    # Confirm instantiation
    b <- blue$exposeObject()
    stopifnot("Lab" %in% class(blue))
    stopifnot(b$name == "blue")
    stopifnot(b$desc == "Blue Lab")
    stopifnot(isTRUE(all.equal(b$parent, nlpStudio)))
    stopifnot((Sys.time() - b$created) < 1)
    stopifnot((Sys.time() - b$modified) < 1)
    stopifnot(dir.exists("./NLPStudio/labs/blue"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/data"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/data/external"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/data/raw"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/data/munge"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/data/processed"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/reports"))
    stopifnot(dir.exists("./NLPStudio/labs/blue/logs"))

    # Logit
    labTests$logs(className = className, methodName = "initiate", msg = paste("Successfully created lab:", b$name))
    cat(paste0(test, " Completed: Success. State is: ", b$state, "!\n"))
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
  }


  init()
  test0()
}

className <- "Lab"

<<<<<<< HEAD
source('./test/unitTests/testNLPStudio.R')
predictifyR <- testLab()
=======
devtools::load_all()
labTests <<- LogTest$new()
testLab()
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
