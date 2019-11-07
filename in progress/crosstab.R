#' @title Two Way Frequency Table
#' @description This function creates a two way frequency table.
#' @param data data frame
#' @param rowvar row factor (unquoted)
#' @param colvar column factor (unquoted)
#' @param type table contains one of: frequencies, cell percents, row percents,
#' or column percents, Default: c("freq", "rowpercent", "colpercent")
#' @param total if TRUE, includes total percents, Default: TRUE
#' @param na.rm if TRUE, deletes cases with missing values, Default: FALSE
#' @param digits number of decimal digits to report for percents, Default: 2
#' @return two way frequency table
#' @details This function creates a two way frequency table that returns a table
#'printed with rows and columns, percents with the percentage sign, a level
#'labeled <NA> if na.rm = FALSE, and a level labeled <Total> if total = TRUE.
#'This function coerces x into a factor if it is not already a factor.
#' @examples crosstab(mtcars, cyl, gear, type=“freq”, digits=2)
#' @rdname crosstab
#' @export

#View(airquality)
crosstab <- function(data, rowvar, colvar,
                     type=c("freq", "rowpercent", "colpercent"),
                     total=TRUE, na.rm=FALSE,
                     digits=2){
  require(dplyr)
  if (!is.factor(rowvar))
    rowvar <- as.factor(rowvar)
  if (!is.factor(colvar))
    colvar <- as.factor(colvar)
  rowvar <- enquo(rowvar)
  colvar <- enquo(colvar)
  t <- table(data$rowvar, data$colvar)
  names(dimnames(tb)) <- c(deparse(substitute(rowvar)), deparse(substitute(colvar)))  if (na.rm==TRUE)
    t <- table(!!data$rowvar, !!data$colvar, useNA = c("ifany"))
  if (total==TRUE)
    t <- addmargins(t, FUN=list("<Total>"=sum))
  if (type=="freq")
    t
  if (type=="rowpercent")
    t <- replace(t, TRUE, sprintf(paste0("%.",digits,"f%%"),prop.table(t,1)*100))
  if (type=="colpercent")
    t <- replace(t, TRUE, sprintf(paste0("%.",digits,"f%%"),prop.table(t,2)*100))
  if (type=="cellpercent")
    t <- replace(t, TRUE, sprintf(paste0("%.",digits,"f%%"),prop.table(t)*100))
  return(t)
}

crosstab(testdata, B, C, type="rowpercent", total=FALSE)

#sample output with test data
library(dplyr)
library(tidyr)

A<-c(1:10, 1:10)
B<-c(NA, 1, 2, 3, NA, 2, 1, 1,2, 3)
C<-c(2, 3, 3, 2, 1, 1, NA, 1, 2, NA)
testdata<-data.frame(A, B, C)

#if na.rm == TRUE
# rowvar = cyl, colvar = gear type = "freq"
t <- addmargins(table(testdata$B, testdata$C, useNA = c("no")),FUN=list("<Total>"=sum))
names(dimnames(t)) <- c("B","C")
t
#if na.rm == FALSE or if na.rm == TRUE | na.rm == FALSE
t <- addmargins(table(testdata$B, testdata$C, useNA = c("ifany")),FUN=list("<Total>"=sum))
names(dimnames(t)) <- c("B","C")
t

# type = "rowpercent" digits = 2
digits = 4
t2 <- replace(t, TRUE, sprintf(paste0("%.",digits,"f%%"),prop.table(t,1)*100))
t2

# type = "colpercent" digits = 2
t3 <- replace(t, TRUE, sprintf("%.2f%%",prop.table(t,2)*100))
t3
# type = "cellpercent" digits=2
t4 <- replace(t, TRUE, sprintf("%.2f%%",prop.table(t)*100))
t4


# print.crosstab
# return matrix
