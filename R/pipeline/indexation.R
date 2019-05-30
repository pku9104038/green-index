## IndexationData class for green index data processing and report generation


## flag for source loaded checking
indexation.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexIndexationData <- setRefClass(
  "GreenIndexIndexationData",
  contains = "GreenIndexStatisticsData",
  
  fields = list(
    
    # database = "GreenIndexDatabase"
    
    
  ),
  
  methods = list(
    
    # Init = function(module.name, config.obj, database.obj){
    #   callSuper(module.name, config.obj)
    #  database <<- database.obj
    # },

    PercentIndex = function() {
      
      if (process[1, kColumnStatisticsAlgorithm] == kAlgorithmPercentIndex) {
        stat.df[, kColumnValue] <<- stat.df[, kColumnValue] / 10
      } else {
        stat.df[, kColumnValue] <<- stat.df[, kColumnValue]
      }
      
      
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
                   # stat.df[, kColumnStatisticsAlgorithm],
                   stat.df[, kColumnStatisticsIndexType],
                   # stat.df[, kColumnValueType],
                   stat.df[, kColumnKey])
      
      for (i in 1: nrow(stat.df)) {
        stat.df[i, kColumnHashDigest] <<- digest(stat.df[i, "msg"], "sha256")
        stat.df[i, kColumnValue] <<- max(1, min(9, floor(stat.df[i, kColumnValue])))
        out.df <<- out.df[out.df[, kColumnHashDigest] != stat.df[i, kColumnHashDigest],]
      }
      stat.df[, "msg"] <<- NULL
        
      out.df <<- rbind(out.df, stat.df)
      
    },
    
    
    PassRateIndex = function() {
      
      stat.df[, kColumnValue] <<- (1 - 2.5 * (100 - stat.df[, kColumnValue]) / 100.0) * 10
      
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
                                 # stat.df[, kColumnStatisticsAlgorithm],
                                 stat.df[, kColumnStatisticsIndexType],
                                 # stat.df[, kColumnValueType],
                                 stat.df[, kColumnKey])
      
      for (i in 1: nrow(stat.df)) {
        stat.df[i, kColumnHashDigest] <<- digest(stat.df[i, "msg"], "sha256")
        stat.df[i, kColumnValue] <<- max(1, min(9, floor(stat.df[i, kColumnValue])))
        out.df <<- out.df[out.df[, kColumnHashDigest] != stat.df[i, kColumnHashDigest],]
      }
      stat.df[, "msg"] <<- NULL
      
      out.df <<- rbind(out.df, stat.df)
      
    }, 
    
    ProcessJob = function(){
      
      if (process[1, kColumnTODO] == "FALSE"){
        return(NA)
        
      } else if (process[1, kColumnTODO] == "TRUE"){
        
        
        stat.df <<- in.df[in.df[, kColumnGrade] == process[1, kColumnGrade] &
                           in.df[, kColumnSubject] == process[1, kColumnSubject] &
                           in.df[, kColumnDomain] == process[1, kColumnDomain] &
                           in.df[, kColumnDimention] == process[1, kColumnDimention] &
                           in.df[, kColumnGroup] == process[1, kColumnGroup] &
                           in.df[, kColumnAttribute] == process[1, kColumnAttribute] &
                            in.df[, kColumnTopic] == process[1, kColumnTopic],  ]
        if (process[1, kColumnKey] != "ALL" && nrow(stat.df) > 0) {
          stat.df <<- stat.df[stat.df[, kColumnKey] == process[1, kColumnKey], ]
        }
        
        if (nrow(stat.df) > 0){
          LogInfo(paste("Process",
                        process[1, kColumnSubject], 
                        process[1, kColumnDomain], 
                        process[1, kColumnDimention],
                        process[1, kColumnGroup],
                        process[1, kColumnAttribute],
                        process[1, kColumnTopic],
                        process[1, kColumnStatisticsAlgorithm],
                        process[1, kColumnKey]))
          
          stat.df[, kColumnStatisticsAlgorithm] <<- process[1, kColumnStatisticsAlgorithm]
          stat.df[, kColumnStatisticsIndexType] <<- process[1, kColumnStatisticsIndexType]
          stat.df[, kColumnValueType] <<- process[1, kColumnValueType]
          
          if (process[1, kColumnStatisticsAlgorithm] == kAlgorithmPercentIndex) {
            PercentIndex()
          } else if (process[1, kColumnStatisticsAlgorithm] == kAlgorithmPassRateIndex) {
            PassRateIndex()
          } else if (process[1, kColumnStatisticsAlgorithm] == kAlgorithmBalanceIndex) {
            PercentIndex()
          }
          
        }

      }
    },
    
    IndexationData = function(){
      LogInfo("Indexation Data!")
      
      # get job configuration
      jobs <- config$GetConfigJob()$indexation
      reworkall <- config$IsReworkAll()
      dropdata <- config$IsDropData()
      DDL <- jobs$DDL
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
          if (dropdata && database$ExistsTable(output.table)) {
            database$RemoveTable(output.table)
          }
          
          # if (database$ExistsTable(output.table)) {
          #  database$RemoveTable(output.table)
          # }
          
          SQL <- paste("CREATE TABLE IF NOT EXISTS", output.table, DDL)
          print(SQL)
          database$CreateTable(output.table, SQL)
          
          out.df <<- database$ReadTable(output.table)
          
          LogInfo(paste("Indexation", input.table, "through", process.table,
                        "into", output.table))
          
          k <- 1
          while (k <= nrow(process.df)) {
            process <<- process.df[k, ]
            stat.df <<- data.frame()
            ProcessJob()
            k <- k + 1
          }
          
          if (nrow(out.df) > 0) {
            database$WriteTable(out.df, output.table)
          }
          
        }
        
      }
      
    }
  )
)
