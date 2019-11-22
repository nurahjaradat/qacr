#' @title Print a crosstab object
#' @description This function prints the results of a calculated two-way
#' frequency table.
#' @param x An object of class \code{crosstab}
#' @param ... Parameters passed to print function
#' @return NULL
#' @examples
#' \dontrun{
#' mycrosstab <- crosstab(mtcars, cyl, gear, type = "freq", digits = 2)
#' print(mycrosstab)
#' mycrosstab <- crosstab(mtcars, cyl, gear, type = "rowpercent", digits = 3)
#' print(mycrosstab)
#' }
#' @rdname print.crosstab
#' @export

print.crosstab <- function(x, ...) {
  if(!inherits(x, "crosstab")) stop("Must be a crosstab")
  if(x$type != "freq")
    x$tb <- replace(x$tb, TRUE, sprintf(paste0("%.",x$digits,"f%%"),x$tb*100))
  print.table(x$tb)
}

