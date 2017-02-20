library(RMySQL)

load_table <- function(tbl_name) {
  con <- dbConnect(MySQL(), default.file = "~/.mylogin.cnf", group = "ergonaut_dev", user = NULL, password = NULL)
  
  query_string <- paste("SELECT * FROM ", tbl_name)
  df <- dbGetQuery(con, query_string)
  dbDisconnect(con)
  return(df)
}

msq.test <- function(fac1, fac2) {
  r <- cor(unclass(fac1), unclass(fac2))
  n <- length(fac1)
  M.sq <- (n - 1) * r^2
  1 - pchisq(M.sq, df = 1)
}

week.abb <- c("Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat")
rec.levels <- c("Reject", "Major Revisions", "Minor Revisions", "Accept")
rec.levels.abbrv <- c("Reject", "Maj. Rev.", "Min. Rev.", "Accept")
dec.levels <- rec.levels


test.report <- function(test, type) {
  str <- NULL
  if (type == "chi.sq") {
    str <- paste("$\\chi^2$(", test$parameter,
               ", *N* = ", sum(test$observed),
               ") = ", round(test$statistic, 2),
               ", *p* = ", round(test$p.value, 2),
               sep = "")
  }
  else if (type == "t") {
    str <- paste("*t*(", round(test$parameter, 2),
                 ") = ", round(test$statistic, 2),
                 ", *p* = ", round(test$p.value, 2),
                 sep = "")
  }
  return(str)
}

perc.frame <- function(tbl) {
  (prop.table(t(tbl), 2) * 100) %>% as.data.frame()
}