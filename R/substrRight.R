#' Saves the last n characters in a string
#'
#' @param x The string.
#' @param n The number of characters at the end of the string to save.
#' @description Saves the last n characters in a string. Copied from http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r.
#'
#' @export

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}