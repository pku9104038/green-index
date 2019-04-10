## CreateTable class for green index data processing and report generation


## flag for source loaded checking
createtable.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexCreateTable <- setRefClass(
  "GreenIndexCreateTable",
  contains = "GreenIndexBase",
  
  fields = list(
    database = "GreenIndexDatabase"
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
    },
    
    CreateTable = function(){
      LogInfo("Create Table!")
      
      jobs <- config$GetConfigJob()$createtable
      reworkall <- config$IsReworkAll()
      reworkjobs <- jobs$TODO
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        if (TODO || reworkjobs || reworkall) {
          
          for (j in 1:length(job$table)) {
            table <- paste0(job$table[j], job$suffix)
            if (job$drop) {
              database$RemoveTable(table)
            }
            DDL <- paste("CREATE TABLE IF NOT EXISTS", table, job$DDL)
            database$CreateTable(table, DDL)
          }
          
          
        }
        
      }
      
    }
    
  )
)

# colnames(df) <- tolower(colnames(df))
# df <- df[!duplicated(df[,c("学科")]), ]