## IndexationData class for green index data processing and report generation


## flag for source loaded checking
loadindex.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexLoadIndex <- setRefClass(
  "GreenIndexLoadIndex",
  contains = "GreenIndexIndexationData",
  
  fields = list(
    
    # database = "GreenIndexDatabase"
    xlsx = "GreenIndexXlsx"
    
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj, xlsx.obj){
      callSuper(module.name, config.obj, database.obj)
      xlsx <<- xlsx.obj
    },
    
    LoadIndex = function(){
      LogInfo("Load external index!")
      
      # get job configuration
      jobs <- config$GetConfigJob()$loadindex
      reworkall <- config$IsReworkAll()
      dropdata <- config$IsDropData()
      reworkjobs <- jobs$TODO
      
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        
        if (TODO || reworkjobs || reworkall) {
          
          table <- paste0(job$table, job$suffix)
          if (dropdata && database$ExistsTable(table)) {
            database$RemoveTable(table)
          }
          
          file <- job$xlsx
          print(file)
          sheets <- job$sheet
          print(sheets)
          for (i in 1:length(sheets)) {
            sheet <- sheets[[i]]
            LogInfo(paste(file, "sheet", sheet, "into", table))
            df <- xlsx$ReadXlsxSheet(file, sheet)
          
            df[, kColumnAssessment] <- config$GetAssessmentName()
            df[, "msg"] <- paste(df[, kColumnAssessment],
                                 df[, kColumnGrade],
                                 df[, kColumnSubject],
                                 df[, kColumnDomain],
                                 df[, kColumnDimention],
                                 df[, kColumnGroup],
                                 df[, kColumnAttribute],
                                 df[, kColumnTopic],
                                 df[, kColumnStatisticsTier],
                                 df[, kColumnStatisticsScope],
                                 df[, kColumnCity],
                                 df[, kColumnDistrict],
                                 df[, kColumnSchool],
                                 df[, kColumnStatisticsPerspective],
                                 df[, kColumnStatisticsSample],
                                 df[, kColumnStatisticsVariable],
                                 # df[, kColumnStatisticsAlgorithm],
                                 df[, kColumnStatisticsIndexType],
                                 # df[, kColumnValueType],
                                 df[, kColumnKey])
            
            for (j in 1:nrow(df)) {
              df[j, kColumnHashDigest] <- digest(df[i,"msg"], algo = "sha256")
              df[j, kColumnTimeStamp] <- as.character(Sys.time())
              
            }
            df[, "msg"] <- NULL
            
            database$AppendTable(df, table)         
            
          }
          
        }
        
      }
      
    }
  )
)
