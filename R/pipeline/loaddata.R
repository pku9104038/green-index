## LoadData class for green index data processing and report generation


## flag for source loaded checking
loaddata.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexLoadData <- setRefClass(
  "GreenIndexLoadData",
  contains = "GreenIndexBase",
  
  fields = list(
    xlsx = "GreenIndexXlsx",
    database = "GreenIndexDatabase"
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj, xlsx.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
      xlsx <<- xlsx.obj
    },
    
    LoadData = function(){
      
      jobs <- config$GetLoadDataJob()
      for (i in 1:length(jobs)){
        job <- jobs[[i]]
        TODO <- job$TODO
        if (TODO || config$IsReworkAll()) {
          
          file <- job$xlsx
          sheet <- job$sheet
          table <- job$table
          
          if (is.element(table, kSubjectSet)) {
            table <- paste0(table, kTableRaw)
          }
          df <- xlsx$ReadXlsxSheet(file, sheet)
          LogInfo(paste(file, sheet, table))
          database$WriteTable(df, table)
        }
        
      }
      
    }
    
  )
)

# colnames(df) <- tolower(colnames(df))
# df <- df[!duplicated(df[,c("学科")]), ]