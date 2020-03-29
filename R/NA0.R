
#' @export
NA0 <- function(x){
  x[is.na(x)] <- 0
  return(x)
}
