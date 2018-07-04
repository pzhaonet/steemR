#' Clear the format of the 'follow' information from steemdb.com.
#'
#' @param x A character string of the follower information from steemdb.com.
#'
#' @return A character string of the formatted follower information
#'
clearferfing <- function(x){
  # remove the spaces
  x1 <- gsub(' ', '', x)
  # remove the line breaker
  x2 <- gsub('\n', '', x1)
  # remove "
  y <- gsub('\\"','', x2)
  y <- substr(y, 2, nchar(y) - 1)
  strsplit(y, ',')[[1]]
}

#' Concert unit from GV, MV, kV to numeric values.
#'
#' @param x the unit GV, MV, kV to convert
#'
#' @return the numeric multiplier.
#'
unitconvert <- function(x){
  switch(x,
         'GV' = 10 ^ 9,
         'MV' = 10 ^ 6,
         'kV' = 10 ^ 3)
}

#' Convert an id from a character to html hyperlink
#'
#' @param id A character string of a Steem ID without '@'.
#'
#' @return A character string of the hyperlink to the Steem id.
#'
#' @examples
#' idlink('dapeng')
idlink <- function(id = NA) {
  paste0('<a href="https://steemit.com/@', id, '">@', id, '</a>')
}


#' Split the characters in a data frame
#'
#' @param dataframe A column of a dataframe with characters to split
#'
#' @return A character vector
#'
#' @examples charsplit()
charsplit <- function(dataframe = NA){
  char_all <- paste(dataframe, collapse = ' ')
  char_single <- strsplit(char_all, split = ' ')[[1]]
}


#' Clear the characters in the titles
#'
#' @param title The post title
#'
#' @return A clear title
clear_title <- function(title){
  title_new <- gsub('"', '\\\\"', title)
  title_new <- gsub('\\|', '-', title_new)
  title_new <- gsub('\\?\\?', '', title_new)
  return(title_new)
}
