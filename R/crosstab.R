#' @title Two Way Frequency Table
#' @description This function creates a two way frequency table.
#' @param data data frame
#' @param rowvar row factor (unquoted)
#' @param colvar column factor (unquoted)
#' @param type table contains one of:c("freq", "rowpercent", "colpercent")
#' representing frequencies, cell percents, row percents, or column percents,
#' Default: "freq"
#' @param total if TRUE, includes total percents, Default: TRUE
#' @param na.rm if TRUE, deletes cases with missing values, Default: FALSE
#' @param digits number of decimal digits to report for percents, Default: 2
#' @return two way frequency table
#' @details This function creates a two way frequency table that returns a table
#' printed with rows and columns, percents with the percentage sign, a level
#' labeled <NA> if na.rm = FALSE, and a level labeled <Total> if total = TRUE.
#' This function coerces x into a factor if it is not already a factor.
#' @examples
#' crosstab(mtcars, cyl, gear, type="freq", digits=2)
#' crosstab(mtcars, cyl, gear, type="rowpercent", digits=3)
#' @rdname crosstab
#' @export

crosstab <- function(data, rowvar, colvar, type="freq",
                     total=TRUE, na.rm=FALSE,
                     digits){
  rowvar <- deparse(substitute(rowvar))
  colvar <- deparse(substitute(colvar))
  if (!is.factor(data[[rowvar]]))
    data[rowvar] <- as.factor(data[[rowvar]])
  if (!is.factor(data[[colvar]]))
    data[colvar] <- as.factor(data[[colvar]])
  tb <- table(data[[rowvar]], data[[colvar]])
  names(dimnames(tb)) <- c(rowvar, colvar)
  if (na.rm==TRUE)
    tb <- table(data[[rowvar]], data[[colvar]], useNA = c("ifany"))
  names(dimnames(tb)) <- c(rowvar, colvar)
  if (type=="freq")
    tb
  if (type=="rowpercent")
    tb <- round(prop.table(tb,1), digits+2)
  if (type=="colpercent")
    tb <- round(prop.table(tb,2), digits+2)
  if (type=="cellpercent")
    tb <- round(prop.table(tb),digits+2)
  if (total==TRUE)
    tb <- addmargins(tb, FUN=list("<Total>"=sum), quiet=TRUE)
  result <- list(type=type, digits=digits, tb=tb)
  class(result) <- c("crosstab","list")
  return(result)
}

