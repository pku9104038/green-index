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
    
    LoadExcel = function(jobs){
      
      reworkall <- config$IsReworkAll()
      reworkjobs <- jobs$TODO
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        if (TODO || reworkjobs || reworkall) {
          
          table <- paste0(job$table, job$suffix)
          if (is.element(table, kSubjectSet)) {
            table <- paste0(table, kTableRaw)
          }
          
          file <- job$xlsx
          sheet <- job$sheet
          if (sheet == "csv") {
            LogInfo(paste(file, "into", table))
            df <- xlsx$ReadCsvFile(file)
          } else {
            LogInfo(paste(file, "sheet", sheet, "into", table))
            df <- xlsx$ReadXlsxSheet(file, sheet)
          }
          
          columns <- colnames(df)
          for (i in 1:length(columns)) {
            # set NULL into "" for all character column
            if (typeof(columns[i]) == "character"){
              df[is.na(df[, columns[i]]), columns[i]] <- ""
            }
            
          }

          df <- ColumnProcessDataFrame(df, job)
          
          database$WriteTable(df, table)
        }
        
      }
      
    },
    
    LoadData = function() {
      LogInfo("Loading Data!")
      jobs <- config$GetConfigJob()$loaddata
      LoadExcel(jobs)
    },
    
    LoadAttribute = function() {
      LogInfo("Loading Attribute!")
      jobs <- config$GetConfigJob()$loadattribute
      LoadExcel(jobs)
    }
  )
)

# colnames(df) <- tolower(colnames(df))
# df <- df[!duplicated(df[,c("学科")]), ]