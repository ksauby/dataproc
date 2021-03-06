#' Convert the format of multiple columns
#' 
#' @param obj obj
#' @param type type
#' @references found function here on 9sep13: http://stackoverflow.com/questions/11261399/function-for-converting-dataframe-column-type
#' 
#' @export

Convert_Magic_Function <- function(obj, type){
  FUN1 <- switch(type,
                 character = as.character,
                 numeric = as.numeric,
                 factor = as.factor)
  out <- lapply(obj, FUN1)
  as.data.frame(out)
}