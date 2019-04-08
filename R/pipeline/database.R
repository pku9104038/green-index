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
      LogDebug(paste("Disconnect from database", database))
      dbDisconnect(conn)
    },
    
    ReadTable = function(table.name){
      Connect()
      LogDebug(paste("Read database table", table.name))
      tabledata <- dbReadTable(conn, table.name)
      Disconnect()
      return(tabledata)
    },
    
    RemoveTable = function(table.name){
      Connect()
      LogDebug(paste("Remove database table", table.name))
      dbRemoveTable(conn, table.name)
      Disconnect()
    },
    
    CreateTable = function(table.name, DDL) {
      Connect()
      LogDebug(paste("create table", table.name))
      dbGetQuery(conn, DDL)
      Disconnect()
    },
    
    GetQuery = function(SQL) {
      #Connect()
      #LogDebug(paste("GetQuery:", SQL))
      result <- dbGetQuery(conn, SQL)
      #Disconnect()
      return(result)
    },
    
    WriteTable = function(data.frame, table.name){
      Connect()
      if (dbExistsTable(conn, table.name)){
        LogDebug(paste("Remove database table", table.name))
        dbRemoveTable(conn, table.name)
      } 
      LogDebug(paste("Write database table", table.name))
      dbWriteTable(conn, table.name, data.frame, 
                   row.names = FALSE, append = FALSE) 
      Disconnect()
    },
    
    AppendTable = function(data.frame, table.name){
      Connect()
      LogDebug(paste("Append database table", table.name))
      dbWriteTable(conn, table.name, data.frame, 
                   row.names = FALSE, append = TRUE) 
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