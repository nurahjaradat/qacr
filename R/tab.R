#' @title Frequency Distribution for a categorical variable
#' @description FUNCTION_DESCRIPTION
#' @param data A dataframe
#' @param x A factor in the dataframe
#' @param sort Sort levels from high to low, Default: FALSE
#' @param maxcat Maximum number of categories to be included.
#' Will combine extra categories into an other category.  Default: NULL
#' @param minp Minimum proportion for a category, combines extra categories into
#' an other category. Cannot specifiy both maxcat and minp. Default: NULL
#' @param na.rm Removes missing values when TRUE, Default: FALSE
#' @param total Include a total category when true, Default: FALSE
#' @param digits Number of digits the percents should be rounded to, Default: 2
#' @return A data.frame
#' @details The function \code{tab} will calculate the frequency distribution
#' for a categorical variable and output a data.frame with three columns:
#' level, n, percent.
#' @examples
#' \dontrun{
#' frequency <- tab(venues, state, sort = TRUE, na.rm = TRUE, maxcat = 6, digits = 3)
#' print(frequency)
#' }
#' @rdname tab
#' @export
#'
tab <- function(data, x, sort = FALSE, maxcat = NULL, minp = NULL, na.rm = FALSE, total = FALSE, digits = 2) {
  if (na.rm) {
    data = na.omit(data)
  }

  x = data[as.character(substitute(x))][[1]]

  if (!is.null(maxcat) & !is.null(minp)){
    stop("Only maxcat or minp should be specified, not both.")
  }


  if (!is.factor(x)) {
    x = as.factor(x)
  }

  t = table(x, useNA = "ifany")
  ns <- as.vector(t)
  cats = names(t)
  props = as.vector(t / sum(t))
  df = data.frame(level = cats, n = ns, percent = props)

  na_row = as.numeric(row.names(df)[is.na(cats)])
  if (!na.rm & length(na_row) != 0) {

    prop_na = df$percent[na_row]
    na_n = df$n[na_row]
    df = df[-na_row, ]
    cats = df$level
    props = df$percent
    ns = df$n
  }

  if (sort) {
    df = data.frame(level = cats, n = ns, percent = props)
    df = df[order(-df$n),]
    cats = df$level
    props = df$percent
    ns = df$n
  }



  if (!is.null(maxcat)) {
    df = data.frame(level = cats, n = ns, percent = props)
    df = df[order(df$n, decreasing = T),]

    cats = df$level
    props = df$percent
    ns = df$n

    if (length(cats) > maxcat) {
      n_other = sum(ns[(maxcat + 1):length(cats)])
      prop_other = sum(props[(maxcat+1):length(cats)])

      cats = cats[1:maxcat]
      levels(cats) = c(levels(cats), "Other")
      cats[length(cats) + 1] = "Other"
      props = props[1:maxcat]
      props[length(props) + 1] = prop_other
      ns = ns[1:maxcat]
      ns[length(ns) + 1] = n_other


    }

  }


  if (!is.null(minp)) {
    if (minp > 1 & minp < 100){
      minp <- minp/100
      warning("minp argument should be less than one. Converting to proportion")
    }
    if (minp > 100){
      stop("minp should be less than one. Argument is too large")
    }
    t_df = data.frame(level = cats, n = ns, percent = props)
    n_other = 0
    prop_other = 0
    times = sum(t_df[["percent"]] > minp)
    df = data.frame(level = as.factor(rep(NA, times = times)), n = rep(NA, times = times), percent = rep(NA, times = times))
    levels(df$level) = levels(cats)
    place = 1

    for (i in 1:nrow(t_df)) {
      x = t_df$percent[i]

      if ( x < minp){
        n_other = n_other + t_df[["n"]][i]
        prop_other = prop_other + t_df[["percent"]][i]

      } else {
        df[place, ] = t_df[i, ]
        place = place + 1
      }

    }


    cats = df$level
    props = df$percent
    ns = df$n
    levels(cats) = c(levels(cats), "Other")
    cats[length(cats) + 1] = "Other"
    props[length(props) + 1] = prop_other
    ns[length(ns) + 1] = n_other



  }

  if (!na.rm & length(na_row) != 0){
    levels(cats) = c(levels(cats), NA)
    cats[length(cats) + 1] = NA
    props[length(props) + 1] = prop_na
    ns[length(ns) + 1] = na_n
  }

  if (total) {
  levels(cats) = c(levels(cats), "Total")
  cats[length(cats) + 1] = "Total"
  props[length(props) + 1] = sum(props)
  ns[length(ns) + 1] = sum(ns)
  }


  df <- data.frame(level = cats, n = ns, percent = round(props, digits = digits))


  class(df) <- c("tab", "data.frame")
  return(df)
}
