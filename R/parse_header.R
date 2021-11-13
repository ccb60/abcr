

info_codes <- tibble::tribble(~'Code',	~'Meaning',
                              'A', 'area',
                              'B', 'book',
                              'C', 'composer',
                              'D', 'discography',
                              'F', 'file url',
                              'G', 'group',
                              'H', 'history',
                              'I', 'instruction',
                              'K', 'key',
                              'L', 'unit note length',
                              'M', 'meter',
                              'm', 'macro',
                              'N', 'notes',
                              'O', 'origin',
                              'P', 'parts',
                              'Q', 'tempo',
                              'R', 'rhythm',
                              'r', 'remark',
                              'S', 'source',
                              's', 'symbol line',
                              'T', 'tune title',
                              'U', 'user defined',
                              'V', 'voice',
                              'W', 'words',
                              'w', 'words',
                              'X', 'reference number',
                              'Z', 'transcription')

# In general, we need to figure out which category each information directive is
# in. Those categories might be better if they are tied to information required
# in musicxml or other tune information formats.

tune_id <- c('X', 'T', 'C', 'A', 'O', 'R')                    # Comes first
playing_info <- c('P', 'V', 'M', 'L', 'Q', 'K')               # Comes last
background_info = c('B', 'D', 'F', 'S', 'G', 'H', 'N', 'Z')
abc_instructions <- c('I', 'm', 'U')
other_info <- c("r", "s", "W", "w")

other_codes <- dplyr::pull(dplyr::filter(info_codes,  ! (Code %in% tune_id |
                                                        Code  %in% playing_info |
                                                        Code %in% background_info |
                                                        Code %in% abc_instructions) ), Code)

# Need to decide on a tune data structure.
# Focus on intervals
# focus on pitches
# Focus on measures
# XML has "partwise" and "timewise" versions.
# XML is more focused on multi-part music
# THE XML <identification> tag contains most of what we would get here,
    #<creator> (Zero or more times) (composer, lyricist, and arranger)
    #<rights> (Zero or more times)
    #<encoding> (Optional)
    #<source> (Optional)
    #<relation> (Zero or more times)  {Related resource}
    #<miscellaneous> (Optional) {Other metadata}

.dispatch_parser <- function(.code, .line) {
  dplyr::case_when(
    .code %in% tune_id ~ parse_tune_info(.line),
    .code %in% playing_info ~ parse_playing_info(.line),
    .code %in% background_info ~ parse_background_info(.line),
    .code %in% abc_instructions~ parse_abc_instructions(.line)
  )
}



parse_tune_info <- function(.lines) {
  print('not yet implemented.')
}

parse_playing_info <- function(.lines) {
  print('not yet implemented.')
}

parse_background_info <- function(.lines) {
  print('not yet implemented.')
}

parse_abc_instructions <- function(.lines) {
  print('not yet implemented.')
}

