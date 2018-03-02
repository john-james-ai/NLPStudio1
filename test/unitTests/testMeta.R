testMeta <- function() {

  test0 <- function() {
    test <- "test0: Create Meta"
    cat(paste0("\n",test, " Commencing\n"))
    
    m <- Meta$new(name = "axel", class(corpus)[1])
    
    # Print default meta data
    print(m$meta)

    cat(paste0(test, " Completed: Success!\n"))

    return(m)
  }
  
  
  test1 <- function(m) {
    test <- "test1: Meta: Validation"
    cat(paste0("\n",test, " Commencing\n"))
    
    key <- c("author", "contributor")
    value <- c("Etta James", "Daniel Day", "Lewis")
    
    m$meta(key,value) # Same length error
    m$meta(key = NULL, value = value) # No key provided
    
    cat(paste0(test, " Completed: Success!\n"))
    
    return(m)
  }
  
  test2 <- function() {
    test <- "test2: Meta: Manage"
    cat(paste0("\n",test, " Commencing\n"))
    #TODO: Test
    # 1. Ability to update a single existing value
    # 2. Add key value pair
    # 3. Add vectors of keys and values
    
    #1 Update single existing entry
    m$meta(key = "name", value = "bryant")
    name <- m$meta(key = "name")
    stopifnot(name == "Bryant Jones")
    
    cat(paste0(test, " Completed: Success!\n"))
    
    return()
  }

  testn <- function() {
    test <- "testn: Meta: Unzip"
    cat(paste0("\n",test, " Commencing\n"))

    cat(paste0(test, " Completed: Success!\n"))

    return()
  }


downloadPath <- "./test/testCorpus/swiftKey/data/external"

init()
corpus0 <<- test0()
corpus1 <<- test1()
test2()


}
className <- "Meta"
#source('./test/unitTests/testMeta.R')
testMeta()
