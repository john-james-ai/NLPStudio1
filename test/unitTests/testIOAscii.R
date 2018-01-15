testIOAscii <- function() {

  # Tests read and write of files containing ASCII control characters
  inDir <- "./test/testData/hc"
  outDir <- "./test/testData/output"
  dir.create(outDir)
  inFiles <- list.files(inDir, full.names = TRUE)
  data <- lapply(inFiles, function(f) {
    readLines(f)
  })

  lapply(seq_along(data), function(x) {
    fileName <- basename(inFiles[x])
    path <- file.path(outDir, fileName)
    writeLines(data[[x]], con = path)
  })
  outFiles <- list.files(outDir, full.names = TRUE)
  for (i in 1:length(inFiles)) {
    stopifnot(all.equal(readLines(inFiles[i]), readLines(outFiles[i])))
  }
}

testIOAscii()
