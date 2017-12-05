testLab <- function() {
  init <- function() {

    # Clean up
    if (exists("blue", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("blue", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("brown", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("brown", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("oxford", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("oxford", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
  }

  # Test 0: Confirm instantiation of lab
  test0 <- function() {
    test <- "test0: Lab Instantiation"
    cat(paste0("\n",test, " Commencing\n"))

    # Test Instantiation
    # Lab$new() # should fail, name is required: Success
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
  }


  init()
  test0()
}

className <- "Lab"

devtools::load_all()
labTests <<- LogTest$new()
testLab()
