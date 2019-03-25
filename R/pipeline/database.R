## DatabaseClass for green index data processing and report generation


## flag for source loaded checking
database.loaded <- TRUE


## define GreenIndexDatabase
library(methods)
library(RPostgreSQL)

GreenIndexDatabase <- setRefClass(
  "GreenIndexDatabase",
  contains = "GreenIndexBase",
  
  fields = list(
    host = "character",
    port = "character",
    database = "character",
    user = "character",
    password = "character",
    driver = "PostgreSQLDriver",
    conn = "PostgreSQLConnection"
  ),
  
  
  methods = list(
    
    Connect = function(){
      conn <<- dbConnect(driver, dbname = database,
                         host = host, port = port,
                         user = user, password = password)
      LogDebug(paste("Connect to database", database))
    },
    Disconnect = function(){
      dbDisconnect(conn)
      LogDebug(paste("Disconnect from database", database))
    },
    
    ReadTable = function(table.name){
      Connect()
      tabledata <- dbReadTable(conn, table.name)
      LogDebug(paste("Read database table", table.name))
      Disconnect()
      return(tabledata)
    },
    
    WriteTable = function(data.frame, table.name){
      Connect()
      if (dbExistsTable(conn, table.name)){
        dbRemoveTable(conn, table.name)
      } 
      dbWriteTable(conn, table.name, data.frame, 
                   row.names = FALSE, append = FALSE) 
      LogDebug(paste("Write database table", table.name))
      Disconnect()
    },
    
    AppendTable = function(data.frame, table.name){
      Connect()
      dbWriteTable(conn, table.name, data.frame, 
                   row.names = FALSE, append = TRUE) 
      LogDebug(paste("Append database table", table.name))
      Disconnect()
    },
    
    RemoveTable = function(table.name){
      Connect()
      dbRemoveTable(conn, table.name)
      LogDebug(paste("Remove database table", table.name))
      Disconnect()
    },
    
    Init = function(module.name, config.obj, user, password){
      callSuper(module.name, config.obj)
      
      host <<- config$GetDatabasehost()
      port <<- config$GetDatabasePort()
      database <<- config$GetDatabaseName()
      user <<- user
      password <<- password
      driver <<- dbDriver("PostgreSQL")
      
    }
    
  )
) 