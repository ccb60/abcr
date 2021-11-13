
#' Split a tune into the header and the tune body
#'
#' @param .tune
#'
#' @return a list with parts `$header` and `$body`.
#' @export
#'
#' @examples
split_parts<- function(.tune) {
  stopifnot(mode(.tune) == 'character')

  # the last row of the tune header is the key, information field K:
  # SHOULD be only one of them in the header, although more can be pulled
  # in the body
  end_of_header <- min(grep("^K:", .tune))
  return(list(header = .tune[1:end_of_header],
              body = .tune[(end_of_header + 1):length(.tune)]))

}

#' Isolate a tune, starting at a specific line.
#'
#' @param .lines
#' @param .start
#'
#' @return
#' @export
#'
#' @examples
isolate_tune <- function(.lines, .start) {
  # TODO:  add a check that the second line is a title line
  if(substr(.lines[[.start]], 1,2) != 'X:') {
    print(substr(.lines[[.start]], 1,2))
    warning('ABC tunes should .start with an "X:" line, but line ', .start,
                 ' is: ', .lines[[.start]])
  }

  # find empty lines and blank (whitespace only) lines
  # Note the following only finds conventional tabs and spaces, not other
  # whitespace, including non-braking spaces.
  blanks <- which(nchar(.lines) == 0)
  invisibles <- which(grepl('^[ \\t]*$', .lines))
  blanks <- c(blanks, invisibles)

  # the tune ends just before the blank line
  #browser()
  end <- min(blanks[blanks>.start]) - 1
  if (is.infinite(end)) {
    end <- length(.lines)
  }
  return(.lines[.start:end])
}

#' Identify the ABC version number from an ABC file
#'
#' @param .lines
#'
#' @return
#' @export
#'
#' @examples
pull_version <- function(.lines) {
  stopifnot(inherits(.lines, 'character'))
  stopifnot(is.vector(.lines))

  line <- .lines[[1]]
  if (!grepl('^%abc', line)) {
    version = sub('[- \t]',  line[f:length(line)])
    # the following assumes decimal version numbers, but version numbers are
    # often period-separated lists of numbers
    version_nums<- strsplit(version, '.')
    if (! is.null(version) && as.numeric(version >= 2.1)){
      interpretation = 'strict'
    } else
      interpretation = 'loose'
  }
}

#' Find the .starts of all tunes within an ABC tune file
#'
#' @param .lines vector of (character) lines from an ABC file, in order.
#'
#' @return An integer vector of .starting locations for each tune, as defined by
#'   the `X:...` tag that begins each tune.
#' @export
#'
#' @examples
find_tunes <- function(.lines) {

  stopifnot(inherits(.lines, 'character'))
  stopifnot(is.vector(.lines))

  # We need to identify .starting points for each song
  .startlines <- grep('^X:', .lines)
  return(.starts = .startlines)
}


split_symbols <- function(.tune_body) {print('Not yet implemented.')}
split_lyrics <- function(.tune_body) {print('Not yet implemented.')}
split_lyrics_fixed <- function(.tune_body) {print('Not yet implemented.')}

