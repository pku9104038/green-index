## ConvergeTable class for green index data processing and report generation


## flag for source loaded checking
convergetable.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexConvergeTable <- setRefClass(
  "GreenIndexConvergeTable",
  contains = "GreenIndexBase",
  
  fields = list(
    database = "GreenIndexDatabase"
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
    },
    
    ConvergeTable = function(){
      LogInfo("Converge Table!")
      
      jobs <- config$GetConfigJob()$convergetable
      reworkall <- config$IsReworkAll()
      reworkjobs <- jobs$TODO
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        if (TODO || reworkjobs || reworkall) {
          
          output.table <- paste0(job$output$table, job$output$suffix)
          LogDebug(output.table)
          database$RemoveTable(output.table)
          input.table <- job$input$table
          
          for (j in 1:length(input.table)) {
            table <- paste0(input.table[[j]]$name, input.table[[j]]$suffix)
            if (database$ExistsTable(table)) {
              df <- database$ReadTable(table)
              if (nrow(df) > 0){
                if (j == 1){
                  database$WriteTable(df, output.table)
                } else {
                  database$AppendTable(df, output.table)
                }
              }
            } else {
              LogError(paste(table, "NOT EXISTS!"))
            }
            
            
            
          }
          
          
        }
        
      }
      
    }
    
  )
)