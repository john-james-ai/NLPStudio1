testParse <- function() {

  init <- function() {
    if (exists("train", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("train", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("val", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("val", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    if (exists("en_US.news.txt", envir = .GlobalEnv))  rm(list = ls(envir = .GlobalEnv)[grep("en_US.news.txt", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    file.copy('./test/testData/en_US.news.txt', './NLPStudios/documents/text/en_US.news.txt', overwrite = TRUE)

  }

  test0 <- function() {
    test <- "test0: Parse: Parsing"
    cat(paste0("\n",test, " Commencing\n"))

    # Instantiate Set
    train <- Set$new(name = 'train', desc = 'Train Set')
    news <- Document$new("./NLPStudios/documents/text/en_US.news.txt", desc = 'News Register')
    train <- train$addDocument(news)
    news <- news$move(train)

    # Pass the set to each command
    parseDigit <- ParseDigitCmd$new(train)
    parsePunct <- ParsePunctCmd$new(train)

    # Pass the commands to the parser
    parser <- Parser$new()
    parser$execute(parseDigit)

    # Execute
    parser$parseDigitCmd$execute()
    parser$execute(parsePunctCmd)


    # Logit
    ParseTest$logs(className = className, methodName = "initialize", msg = paste("Successfully initialized set", s$name))
    cat(paste0(test, " Completed: Success!\n"))

    return(train)
  }


init()
train <- test0()

}
className <- "Set"
ParseTest <- LogTest$new()
#source('./test/unitTests/testCorpusBuilder.R')
testParse()
