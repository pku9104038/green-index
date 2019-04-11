## QueryData class for green index data processing and report generation


## flag for source loaded checking
querydata.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexQueryData <- setRefClass(
  "GreenIndexQueryData",
  contains = "GreenIndexBase",
  
  fields = list(
    
    database = "GreenIndexDatabase"
    
    
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
    },
    
    TenLevelIndex = function() {
      
      stat.df[, kColumnValue] <<- floor(stat.df[, kColumnValue]/10)
      stat.df[stat.df[, kColumnValue] == 0, kColumnValue] <<-1
      stat.df[, "msg"] <<- paste(stat.df[, kColumnAssessment],
                                 stat.df[, kColumnGrade],
                                 stat.df[, kColumnSubject],
                                 stat.df[, kColumnDomain],
                                 stat.df[, kColumnDimention],
                                 stat.df[, kColumnGroup],
                                 stat.df[, kColumnAttribute],
                                 stat.df[, kColumnTopic],
                                 stat.df[, kColumnStatisticsTier],
                                 stat.df[, kColumnStatisticsScope],
                                 stat.df[, kColumnStatisticsPerspective],
                                 stat.df[, kColumnStatisticsSample],
                                 stat.df[, kColumnStatisticsVariable],
                                 stat.df[, kColumnStatisticsAlgorithm],
                                 stat.df[, kColumnStatisticsOutcome],
                                 stat.df[, kColumnValueType],
                                 stat.df[, kColumnKey])
      
      for (i in 1: nrow(stat.df)) {
        stat.df[i, kColumnHashDigest] <<- digest(stat.df[i, "msg"], "sha256")
      }
      #stat.df[, kColumnHashDigest] <<- digest(stat.df[, "msg"], "sha256")
      stat.df[, "msg"] <<- NULL
      
      
      # out.df <<- rbind(out.df, stat.df)
      
    },
    
    ProcessJob = function(){
      
      if (process[1, kColumnTODO] == "FALSE"){
        return(NA)
        
      } else if (process[1, kColumnTODO] == "TRUE"){
        LogInfo(paste("Process",
                      process[1, kColumnDomain], 
                      process[1, kColumnDimention],
                      process[1, kColumnGroup],
                      process[1, kColumnAttribute],
                      process[1, kColumnTopic],
                      process[1, kColumnStatisticsTier],
                      process[1, kColumnStatisticsPerspective],
                      process[1, kColumnStatisticsAlgorithm],
                      process[1, kColumnKey]))
        
        stat.df <<- in.df[in.df[, kColumnGrade] == process[1, kColumnGrade] &
                            in.df[, kColumnSubject] == process[1, kColumnSubject] &
                            in.df[, kColumnDomain] == process[1, kColumnDomain] &
                            in.df[, kColumnDimention] == process[1, kColumnDimention] &
                            in.df[, kColumnGroup] == process[1, kColumnGroup] &
                            in.df[, kColumnAttribute] == process[1, kColumnAttribute] &
                            in.df[, kColumnGroup] == process[1, kColumnGroup] &
                            in.df[, kColumnTopic] == process[1, kColumnTopic],  ]
        if (process[1, kColumnKey] != "ALL" && nrow(stat.df) > 0) {
          stat.df <<- stat.df[stat.df[, kColumnKey] == process[1, kColumnKey], ]
        }
        
        if (nrow(stat.df) > 0){
          stat.df[, kColumnStatisticsAlgorithm] <<- process[1, kColumnStatisticsAlgorithm]
          stat.df[, kColumnStatisticsOutcome] <<- process[1, kColumnStatisticsOutcome]
          stat.df[, kColumnValueType] <<- process[1, kColumnValueType]
          
          if (process[1, kColumnStatisticsAlgorithm] == kAlgorithmTenLevelIndex) {
            TenLevelIndex()
          }
          
          InsertStatDataframe()
          
        }
        
      }
    },
    
    IndexationData = function(){
      LogInfo("Indexation Data!")
      
      # get job configuration
      jobs <- config$GetConfigJob()$indexation
      reworkall <- config$IsReworkAll()
      reworkjobs <- jobs$TODO
      
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        
        if (TODO || reworkjobs || reworkall) {
          
          # read input, process data table
          input.table.name <- job$input$table
          input.table.suffix <- job$input$suffix
          input.table <- paste0(job$input$table, job$input$suffix)
          in.df <<- database$ReadTable(input.table)
          in.df <<- ColumnProcessDataFrame(in.df, job$input)
          
          
          process.table <- paste0(job$process$table, job$process$suffix)
          process.df <- database$ReadTable(process.table)
          process.df <- process.df[
            process.df[, kColumnTableName] == input.table.name, 
            ]
          process.df <- process.df[
            process.df[, kColumnTableSuffix] == input.table.suffix,
            ]
          
          
          output.table <<- paste0(job$output$table, job$output$suffix)
          # out.df <<- data.frame()
          
          LogInfo(paste("Indexation", input.table, "through", process.table,
                        "into", output.table))
          
          k <- 1
          while (k <= nrow(process.df)) {
            process <<- process.df[k, ]
            stat.df <<- data.frame()
            ProcessJob()
            k <- k + 1
          }
          
          # database$WriteTable(out.df, output.table)
        }
        
      }
      
    }
  )
)
