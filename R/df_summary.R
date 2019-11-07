#' @title df_summary
#' @description A comprehensive description of a data frame
#' @param df a data frame
#' @param digits – number of decimal digits for statistics, Default: 2
#' @return the data data frame invisibly (so that it can be used in a pipeline)
#' @details prints a comprehensive description of a data frame via several tables,
#' a general summary table and tables that provide a breakdown of quantitative and categorical variables
#' @examples
#' data<-data.frame("height"=c(4,5,3,2, 100), 'weight'=c(39,88,NA,15, -2),"names"=c('Bill',"Dean", "Sam", NA, "Jane"), 'race'=c('b','w','w','o', 'b'))
#' df_summary(mtcars)
#' @rdname df_summary
#' @import dplyr
#' @import readr
#' @import e1071
#' @export

df_summary <- function(df, digits = 2){
  if(!(is.data.frame(df))){
    stop("You need to input a data frame")
  }
  general_summary <- function(df, digits=digits){
    varnames <- colnames(df)
    tbl <- matrix( nrow = length(varnames), ncol = 6, byrow = TRUE)
    colnames(tbl) <- c("pos", "variable", "type", "n_unique", "n_missing", "pct_missing")
    rownames(tbl) <- varnames
    tbl[ ,1 ] <- c(1 : length(varnames))
    tbl[ , 2] <- varnames
    for(i in 1:length(varnames)){
      tbl[i,3] <- class(df[,i])
    }
    for(i in 1:length(varnames)){
      tbl[i,4] <- length(unique(df[,i]))
    }
    for(i in 1:length(varnames)){
      tbl[i,5] <- sum(is.na(df[,i]))
    }
    n <- nrow(df)
    for(i in 1:length(varnames)){
      tbl[i,6] <- paste0(round(sum(is.na(df[,i]))*100/n, digits=digits) , "%")
    }
    cat("\nOverall\n",
        "====================================================\n", sep="")
    print(as.table(tbl))
  }
  variable_summary<-function(data, digits=digits){
    cdf<-data.frame()
    ndf<-data.frame()
    for (i in names(data)) {
      numeric<-c()
      categorical<-c()
      if(is.numeric(data[[i]])|is.integer(data[[i]])){
        numeric<-append(numeric, i)
        table1<-data%>%
          select(numeric)
        for (i in 1:length(table1)){
          x<-unlist(table1[,i])
          x<-na.omit(x)
          table_n<-table1%>%
            select(numeric[i])%>%
            na.omit()%>%
            summarise(name=numeric[i], n=sum(!is.na((x))),
                      mean=round(mean(x), digits=digits),
                      sd=round(sd(x), digits=digits),
                      skew=round(skewness(x), digits=digits),
                      p0=min(x),
                      p25=quantile(x, 0.25),
                      p50=median(x),
                      p75=quantile(x, 0.75),
                      p100=max(x))%>%
            mutate(n_outlier=sum((x>p75+(1.5*(p75-p25)))|
                                   x<p25-(1.5*(p75-p25))))
          ndf<-rbind(ndf, table_n)
        }
      }
      if(is.character(data[[i]])|is.factor(data[[i]])){
        categorical<-append(categorical, i)
        table2<-data%>%
          select(categorical)
        for (i in 1:length(table2)){
          table_c<-table2%>%
            na.omit()%>%
            group_by(.dots=categorical[i])%>%
            summarise(variable=categorical[i], n=n())%>%
            mutate(pct=round(n/sum(n), digits=digits))

          colnames(table_c)<-c("level","variable",'n', 'pct')
          table_c<- table_c[c("variable", "level", "n", 'pct')]

          if (nrow(table_c)>10){
            table_c$level<-as.character(table_c$level)
            row_11<-c(variable=categorical[i], level="<...>", n=sum(table_c$n[11:nrow(table_c)]),
                      pct=sum(table_c$pct[11:nrow(table_c)]))
            table_c = table_c[1:10,]
            table_c= rbind(table_c, row_11)
            cdf<-rbind(cdf, table_c)}
          else{
            cdf<-rbind(cdf, table_c)
          }

          cdf$variable[duplicated(cdf$variable)] <- " "
        }
      }
    }
    if (nrow(ndf)>0){
      cat("\nQuatitative Variables\n",
          "====================================================\n", sep="")
      print (data.frame(ndf))
    }
    if (nrow(cdf)>0){
      cat("\nCategorical Variables\n",
          "====================================================\n", sep="")
      print (data.frame(cdf))
    }
  }

  general_summary(df, digits)
  variable_summary(df, digits)
}

