library(RMySQL)

load_table <- function(tbl_name) {
  con <- dbConnect(dbConnect(MySQL(), default.file = "~/.mylogin.cnf", group = "ergonaut_dev", user = NULL, password = NULL))
  
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

week.abb = c("Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat")