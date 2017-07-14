# libraies
library(yaml)
library(RPostgreSQL)


# global variables
g.db <- yaml.load_file("yaml/conf.yaml")$db
g.db$drv <- dbDriver("PostgreSQL")
g.db$con <- dbConnect(g.db$drv, dbname = g.db$dbname,
                      host = g.db$host, port = g.db$port,
                      user = g.db$user, password = g.db$password)

# get database  global  variables
db.Global <- function(){
  return(g.db)
}

# get csv storage path
data.path <- function(
  table
){
  return(
    paste0(g.dir$data,g.db$name,"/",table,".csv")
  )
}

#  disconnect  database
db.Disconnect <- function(){
  db <- db.Global()
  if(db$storage=="postgre"){
    db$con <- dbDisconnect(g.db$con)
  }
  return(g.db$con)
}

# read a table
db.ReadTable <- function(
  table
){
  db <- db.Global()
  result <- NULL
  if(db$storage == "postgre"){
    result <- dbReadTable(conn = db$con, 
                          name = table)
  }
  else if(db$storage == "csv"){
    result <- read_csv(file = data.path(table))
  }
  
  return(result)
  
}

# write a table
db.WriteTable <- function(
  data,
  table,
  append = FALSE
){
  db <- db.Global()
  result <- NULL
  if(db$storage == "postgre"){
    if(append == FALSE & 
       dbExistsTable(conn = db$con,
                     name = table)){
      
      dbRemoveTable(conn = db$con, 
                    name = table)
      
    } 
    result <- dbWriteTable(conn = db$con, 
                           name = table, 
                           value = data, 
                           append = append)
  }
  else if(db$storage == "csv"){
    result <- write_csv( x = data,
                         path = data.path(table),
                         append = append)
  }
  
  return(result)
}

#  remove a table
db.RemoveTable <- function(
  table
){
  db <- db.Global()
  result <- NULL
  if(db$storage == "postgre"){
    
    result <- dbRemoveTable(conn = db$con, 
                            name = table)
  }
  else if(db$storage == "csv"){
    result <- unlink(x = data.path(table))
  }
  
  return(result)
  
}

###########################################