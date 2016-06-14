# install.packages("RPostgreSQL")

fetchData<-function(){
  # create a connection
  # save the password that we can "hide" it as best as we can by collapsing it
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, dbname = "postgres",
                   host = "localhost", port = 5432,
                   user = sql_user, password = sql_pass)
  parsedQuery <- paste0("SELECT * from docs")
  clsTree <- dbGetQuery(con, parsedQuery)
  dbDisconnect(con) 
  return(clsTree)
}
