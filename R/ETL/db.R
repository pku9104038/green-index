# libraies
library(yaml)
library(RPostgreSQL)


# global variables
g.db <- yaml.load_file("yaml/conf.yaml")$db
g.db$drv <- dbDriver("PostgreSQL")


# get database  global  variables
db.Global <- function(){
  return(g.db)
}

# get csv storage path
data.path.in <- function(
  table
){
  return(
    paste0(g.dir$data.in,g.db$dbname,"/",table,".csv")
  )
}

# get csv storage path
data.path.out <- function(
  table
){
  return(
    paste0(g.dir$data.out, g.db$dbname,"/",table,".csv")
  )
}

#  disconnect  database
db.Connect <- function(){
  db <- db.Global()
  db$con <- dbConnect(db$drv, dbname = db$dbname,
                        host = db$host, port = db$port,
                        user = db$user, password = db$password)
  return(db)
}


#  disconnect  database
db.Disconnect <- function(
  db,
  storage =  "postgre"
  ){
  #db <- db.Global()
  if(storage=="postgre"){
    db$con <- dbDisconnect(db$con)
  }
  return(db)
}

# read a table
db.ReadTable <- function(
  table,
  storage = "postgre"
){
  #db <- db.Global()
  result <- NULL
  
  if(storage == "postgre"){
    db <-  db.Connect()
    result <- dbReadTable(conn = db$con, 
                          name = table)
    db <- db.Disconnect(db)
  }
  else if(storage == "csv"){
    result <- read_csv(file = data.path.in(table))
  }
  
  return(result)
  
}

# write a table
db.WriteTable <- function(
  data,
  table,
  append = FALSE,
  storage = "postgre"
){
  #db <- db.Global()
  result <- NULL
  
  if(storage == "postgre"){
    db <- db.Connect()
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
    db <- db.Disconnect(db)
  }
  else if(storage == "csv"){
    result <- write_csv( x = data,
                         path = data.path.out(table),
                         append = append)
  }
  
  
  return(result)
}

#  remove a table
db.RemoveTable <- function(
  table,
  storage = "postgre"
){
  #db <- db.Global()
  result <- NULL
  db <- db.Connect()
  if(storage == "postgre"){
    
    result <- dbRemoveTable(conn = db$con, 
                            name = table)
  }
  else if(storage == "csv"){
    result <- unlink(x = data.path.out(table))
  }
  
  db.Disconnect(db)
  return(result)
  
}

###########################################
db.DumpTable <- function(
  table,
  path
){
  data <- db.ReadTable(table)
  write.csv( x = data,file = path)
}
###########################################