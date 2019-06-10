get_hist_data <- function(object) {
  
  d <- diff(object$breaks)[1]

  df <- data.frame(
    x = object$mids,
    y = object$counts, 
    name = sprintf("(%s, %s]",
                   object$mids - d / 2,
                   object$mids + d / 2))
  
}