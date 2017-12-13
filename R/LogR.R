#==============================================================================#
#                                 LogR                                         #
#==============================================================================#
#' LogR
#'
#' \code{LogR} Writes to log
#'
#' Writes to log
#'
#' @section Class methods:
#'
#' \strong{LogR Core Methods:}
#'  \itemize{
#'   \item{\code{new()}}{Initiates thelogobject. }
#'   \item{\code{writeLog()}}{Writes log.}
#'   \item{\code{queryLog(...)}}{Enables client to perform queries on the log.}
#' }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
LogR <- R6::R6Class(
  classname = "LogR",
  lock_objects = FALSE,
  lock_class = TRUE,

  private = list(
    ..entries = list(),
    ..logPath = './NLPStudio/logs',
    notifyInfo  = function(note) futile.logger::flog.info(note, name = "green"),
    notifyWarn  = function(note) futile.logger::flog.warn(note, name = "yellow"),
    notifyError = function(note)  futile.logger::flog.error(note, name = "red")
  ),

  public = list(
    entry = list(
      owner = character(),
      className = character(),
      methodName = character(),
      level = character(),
      msg = character(),
      fieldName = character(),
      created = character()
    ),

    initialize = function(logPath = NULL) {

      if (is.null(logPath)) {
        logPath <- private$..logPath
      }

      dir.create(logPath, showWarnings = FALSE, recursive = TRUE)
      futile.logger::flog.threshold(INFO)
      futile.logger::flog.logger("green", INFO, appender=appender.file(file.path(logPath, "green.log")))
      futile.logger::flog.logger("yellow", WARN, appender=appender.tee(file.path(logPath, "yellow.log")))
      futile.logger::flog.logger("red", ERROR, appender=appender.tee(file.path(logPath, "red.log")))

      invisible(self)
    },

    writeLog  = function() {

      if (is.null(self$entry$owner) | is.null(self$entry$className) |
          is.null(self$entry$methodName) | is.null(self$entry$level) |
          is.null(self$entry$msg)) {

        note <- paste("The usage for the LogR class is",
                      "writeLog(owner, className, methodName,
                      path, level, msg, fieldName = NULL)")
        private$notifyError(note)
      } else {
        level <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
                      self$entry$level, perl = TRUE)

        note <- paste0(level, " in class '",
                       self$entry$className, "', method '", self$entry$methodName, ". ",
                       ifelse(is.na(self$entry$fieldName), "",
                              paste0("with variable '",
                              self$entry$fieldName, "'. ")), self$entry$msg)

        switch(level,
               Info  = private$notifyInfo(note),
               Warn  = private$notifyWarn(note),
               Error = private$notifyError(note)
        )

        # Append to list
        if (is.null(private$..entries)) {
          private$..entries <- self$entry
        }  else {
          private$..entries <- c(private$..entries, self$entry)
        }
      }
    }
  )
)
