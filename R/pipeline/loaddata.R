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
    
    LoadData = function(){
      
      data.source <- config$GetLoadDataJob()
      for (i in 1:length(data.source)){
        
        file <- data.source[[i]]$xlsx[1]
        sheet <- data.source[[i]]$sheet[1]
        table <- data.source[[i]]$table[1]
        TODO <- data.source[[i]]$TODO[1]
        
        if (TODO) {
          if (is.element(table, kSubjectSet)) {
            table <- paste0(table, kTableRaw)
          }
          df <- xlsx$ReadXlsxSheet(file, sheet)
          LogInfo(paste(file, sheet, table))
          database$WriteTable(df, table)
        }
        
      }
      
    },
    
    Init = function(module.name, config.obj, database.obj, xlsx.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
      xlsx <<- xlsx.obj
    }
    
  )
)

# colnames(df) <- tolower(colnames(df))
# df <- df[!duplicated(df[,c("学科")]), ]