#' Examples of simple ABC tunes
#'
#' Several ABC-formatted tunes are loaded as part of the package. They are
#' accessed by file names, stripped of the ".abc" filetype.  Accessing the tunes
#' this way provides a data structure consisting of a list of lines of text.
#'
#' Available tunes include:
#'
#' `r paste(list.files(file.path(getwd(), 'inst/extdata'), ".abc"), paste = '\n')`
#'
#' @aliases abc_tunes
#'
"tennessee_wagoner"
