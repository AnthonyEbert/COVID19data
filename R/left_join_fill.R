
#' @export
left_join_fill <- function(x, y, by = NULL){
  output <- left_join(x,y, by = by, suffix = c(".y", ".x"))

  repeats <- tidyselect::vars_select(names(output), ends_with(".x"))
  repeats_no_x <- stringr::word(repeats, sep = "\\.")

  for(i in 1:length(repeats)){
    output[repeats_no_x[i]] <- coalesce(output[[repeats[i]]], output[[repeats_no_x[i]]])
  }

  other_names <- tidyselect::vars_select(names(output), include = !(ends_with(".x") | ends_with(".y")))

  output <- output %>% select(!!!as.character(other_names))

  return(output)
}
