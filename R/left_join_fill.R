
#' @export
left_join_fill <- function(x, y, by = NULL){
  output <- left_join(x,y, by = by, suffix = c(".x", ".y"))

  repeats_x <- tidyselect::vars_select(names(output), ends_with(".x"))
  repeats_y <- tidyselect::vars_select(names(output), ends_with(".y"))
  repeats_no_x <- stringr::word(repeats_x, sep = "\\.")

  for(i in 1:length(repeats_no_x)){
    output[repeats_no_x[i]] <- coalesce(as.numeric(output[[repeats_x[i]]]), as.numeric(output[[repeats_y[i]]]))
  }

  other_names <- tidyselect::vars_select(names(output), include = !(ends_with(".x") | ends_with(".y")))

  output <- output %>% select(!!!as.character(other_names))

  return(output)
}
