ctrl <- read.csv(file = "./data-raw/ctrl.csv")
encodings <- read.csv(file = "./data-raw/encodings.csv")
devtools::use_data(ctrl, encodings, internal = TRUE, overwrite =  TRUE)
