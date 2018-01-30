ctrl <- read.csv(file = "./ctrl.csv")
encodings <- read.csv(file = "./encodings.csv")
devtools::use_data(ctrl, encodings, internal = TRUE, overwrite =  TRUE)
