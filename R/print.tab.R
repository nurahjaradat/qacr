#' @title Print a tab object
#' @description Print the results of calculating a frequency table
#' @param x An object of class \code{tab}
#' @param ... Parameters passed to a function
#' @return NULL
#' @examples
#' \dontrun{
#' frequency <- tab(venues, type, sort = TRUE, na.rm = FALSE)
#' print(frequency)
#' }
#' @rdname print.tab
#' @export

print.tab <- function(df, ...) {
  if(!inherits(df, "tab")) stop("Must be class 'tab'")
  df$percent = paste(as.character(df$percent*100), "%", sep = "")
  print.data.frame(df)
}

