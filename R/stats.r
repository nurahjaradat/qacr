library(tidyverse)

## Statistics
median <- function(xs){
  ys <- sort(xs)
  m <- length(xs)/2
  if(length(xs) %% 2 == 0){
    mean(ys[m], ys[m+1])
  } else {
    ys[floor(m) + 1]
  }
}

n <- function(xs){
  length(xs)
}

## Auxiliary functions
my_sum <- function(data, col, cus_sum) {
  col <- enquo(col)
  cus_sum_name <- cus_sum
  cus_sum <- rlang::as_function(cus_sum)

  data %>%
    summarise(!!cus_sum_name := cus_sum(!!col))
}

my_sums <- function(data, col, cus_sums) {
  col <- enquo(col)
  map_dfc(cus_sums, my_sum, data = data, col = !!col)
}



#' @title stats
#' @description Descriptive statistics for a quantitative variable
#' @param data data frame
#' @param x numeric variable in data (unquoted)
#' @param statistics statistics to calculate (any function that produces a
#'  value), Default: c("n", "mean", "sd")
#' @param na.rm if TRUE, delete cases with missing values on x and or grouping
#'  variables, Default: TRUE
#' @param digits number of decimal digits to print, Default: 2
#' @param ... list of grouping variables
#' @return a data frame, where columns are grouping variables (optional) and statistics
#' @examples
#' stats(mtcars, mpg, c("n", "mean", "sd"), na.rm = TRUE, digits = 0, am, gear)
#' @rdname stats
#' @export
stats <- function(data, x,
                  statistics = c("n", "mean", "sd"),
                  na.rm = TRUE,
                  digits = 2,
                  ...){
  x <- enquo(x)
  dots <- enquos(...)

  if(!is.numeric(data %>% pull(!!x))){
    stop("data$x is not numeric")
  }

  data <- data %>% select(!!x, !!!dots)

  if(na.rm){
    data <- na.omit(data)
  }

  data %>%
    mutate_at(vars(!!!dots), as_factor) %>%
    group_by(!!!dots) %>%
    group_modify(~my_sums(.x, col = !!x, cus_sums = statistics)) %>%
    ungroup() %>%
    mutate_if(is.double, round, digits)

}
