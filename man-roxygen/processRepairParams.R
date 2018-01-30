#' @param substitutions A two columnn data frame. The first column should contain the decimal representations of the control characters to replace.  The second column should contain the hexadecimal representation of the character to substitute. If the parameter is not provided, the following default replacements will occur
#' \itemize{
#'  \item Control Characters.  The NULL, SUB, and DEL control characters will be replaced with the space character
#'  \item Encodings. A list of common UTF-8 characters that are misinterpreted as Windows-1252 characters are replaced with their Latin character equivalents.
#' }
