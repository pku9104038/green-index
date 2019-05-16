## ScoreConverge class for green index data processing and report generation


## flag for source loaded checking
scoreconverge.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexScoreConverge <- setRefClass(
  "GreenIndexScoreConverge",
  contains = "GreenIndexStatisticsData",
  
  fields = list(
    # database = "GreenIndexDatabase"
  ),
  
  methods = list(
    
    # Init = function(module.name, config.obj, database.obj){
    #   callSuper(module.name, config.obj)
    #   database <<- database.obj
    # },
    
    PreScoreConverge = function() {
      
      jobs <- config$GetConfigJob()$scoreconverge
      reworkall <- config$IsReworkAll()
      reworkjobs <- jobs$TODO
      TODO <- FALSE
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        if (TODO) {
          break
        }
      }
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
          
        if (TODO || reworkjobs || reworkall) {
          
          output.table <<- paste0(jobs$output$table, jobs$output$suffix)
          LogDebug(output.table)
          database$RemoveTable(output.table)
          input.table <- jobs$input$table
          
          for (j in 1:length(input.table)) {
            table <- paste0(input.table[[j]]$name, input.table[[j]]$suffix)
            
            df <- database$ReadTable(table)
            if (nrow(df) > 0){
              if (j == 1){
                database$WriteTable(df, output.table)
              } else {
                database$AppendTable(df, output.table)
              }
            }
          
          }
        }
        
      }
      
    },
    
    FilterData = function() {
      filter.df <<- filter.df[filter.df[, kColumnDomain] == 
                                process[1, kColumnDomain], ]
      if (nrow(filter.df) == 0) {
        LogError(paste(kColumnDomain, process[1, kColumnDomain]))
      } else {
        filter.df <<- filter.df[filter.df[, kColumnDimention] == 
                                  process[1, kColumnDimention], ]
        if (nrow(filter.df) == 0) {
          LogError(paste(kColumnDimention, process[1, kColumnDimention]))
        } else {
          filter.df <<- filter.df[filter.df[, kColumnGroup] == 
                                    process[1, kColumnGroup], ]
          if (nrow(filter.df) == 0) {
            LogError(paste(kColumnGroup, process[1, kColumnGroup]))
            
          } else {
            filter.df <<- filter.df[filter.df[, kColumnAttribute] == 
                                      process[1, kColumnAttribute], ]
            if (nrow(filter.df) == 0) {
              LogError(paste(kColumnAttribute, process[1, kColumnAttribute]))
              
            } else {
              filter.df <<- filter.df[filter.df[, kColumnTopic] == 
                                        process[1, kColumnTopic], ]
              if (nrow(filter.df) == 0) {
                LogError(paste(kColumnTopic, process[1, kColumnTopic]))
                
              } else {
                filter.df <<- filter.df[filter.df[, kColumnStatisticsVariable] ==
                                          process[1, kColumnVariableName], ]
                if (nrow(filter.df) == 0) {
                  LogError(paste(kColumnStatisticsVariable, 
                                 process[1, kColumnVariableName]))
                  
                } else {
                  filter.df <<- filter.df[filter.df[, kColumnKey] == 
                                            process[1, kColumnKey], ]
                  if (nrow(filter.df) == 0) {
                    LogError(paste(kColumnKey, process[1, kColumnKey]))
                  } else {
                    if (process[1, kColumnSubject] == kStringAll) {

                    } else {
                      subjects <- unlist(strsplit(process[1, kColumnSubject], 
                                                  kSeparator))
                      subject.df <- data.frame()
                      for (i in 1:length(subjects)) {
                        subject <- subjects[i]
                        sub.df <- filter.df[filter.df[, kColumnSubject] == 
                                              subject, ]
                        subject.df <- rbind(subject.df, sub.df)
                      }
                      filter.df <<- subject.df
                      if (nrow(filter.df) == 0) {
                        LogError(paste(kColumnSubject, 
                                       process[1, kColumnSubject]))
                      }
                    }
                  }
                }
              }
            }
          }
        } 
      }
      
      return(filter.df)
    },
    
    ProcessJob = function(){
      
      if (process[1, kColumnTODO] == "FALSE" &&  !config$IsReworkAll()){
        return(NA)
        
      } else if (process[1, kColumnTODO] == "TRUE"){
        
        LogInfo(paste("Process",
                      process[1, kColumnSubject], 
                      process[1, kColumnDomain], 
                      process[1, kColumnDimention],
                      process[1, kColumnGroup],
                      process[1, kColumnAttribute],
                      process[1, kColumnTopic],
                      process[1, kColumnStatisticsTier],
                      process[1, kColumnStatisticsPerspective],
                      process[1, kColumnVariableName],
                      process[1, kColumnKey],
                      process[1, kColumnStatisticsAlgorithm]))
        
        InitStatList()
        stat.list[kColumnSubject] <<- process[1, kColumnSubjectConverge]
        variable.name <<- process[1, kColumnSubjectConverge]
        choice.name <<- process[1, kColumnKey]
        
        algorithm <<- process[1, kColumnStatisticsAlgorithm]
        
        filter.df <<- in.df
        
        FilterData()
        
        if (nrow(filter.df) > 0) {
          if (process[1, kColumnStatisticsTier] == kStringAll) {
            tiers <- unlist(unique(filter.df[, kColumnStatisticsTier]))
          } else {
            tiers <- unlist(strsplit(process[1, kColumnStatisticsTier], 
                                     kSeparator))
          }
          
          for (i in 1:length(tiers)) {
            tier.name <<- tiers[i]
            tier.df <- filter.df[
              filter.df[, kColumnStatisticsTier] == tier.name, ]
            if (process[1, kColumnStatisticsPerspective] == kStringAll) {
              perspectives <- unlist(
                unique(tier.df[, kColumnStatisticsPerspective]))
            } else {
              perspectives <- unlist(
                strsplit(process[1, kColumnStatisticsPerspective], kSeparator))
            }
            
            for (j in 1:length(perspectives)) {
              perspective.name <<- perspectives[j]
              perspective.df <- tier.df[
                tier.df[, kColumnStatisticsPerspective] == perspective.name, ]
              if (process[1, kColumnStatisticsScope] == kStringAll) {
                scopes <- unlist(unique(perspective.df[, kColumnStatisticsScope]))
              } else {
                scopes <- unlist(
                  strsplit(process[1, kColumnStatisticsScope], kSeparator))
              }
              
              for (k in 1:length(scopes)) {
                scope.name <<- scopes[k]
                scope.df <- perspective.df[
                  perspective.df[, kColumnStatisticsScope] == scope.name, ]
                if (process[1, kColumnStatisticsSample] == kStringAll) {
                  samples <- unlist(unique(scope.df[, kColumnStatisticsSample]))
                } else {
                  samples <- unlist(
                    strsplit(process[1, kColumnStatisticsSample], kSeparator))
                }
                
                for (l in 1:length(samples)) {
                  sample.name <<- samples[l]
                  sample.df <<- scope.df[
                    scope.df[, kColumnStatisticsSample] == sample.name, ]
                  
                  if (algorithm == kAlgorithmMean) {
                    statistics.value <<- 
                      mean(as.numeric(sample.df[, kColumnValue]), na.rm = TRUE)
                  }
                  AddStatDataframe()
                  
                }
              }
            }
          }
          
          InsertStatDataframe() 
        } 
        
        
      }
    },
    
    
    ScoreConverge = function(){
      LogInfo("Score Converge!")
      
      PreScoreConverge()
      
      jobs <- config$GetConfigJob()$scoreconverge
      reworkall <- config$IsReworkAll()
      dropdata <- config$IsDropData()
      DDL <- jobs$DDL
      reworkjobs <- jobs$TODO
      
      district.table <- paste0(jobs$info$district$table, 
                               jobs$info$district$suffix)
      district.df <<- database$ReadTable(district.table)
      school.table <- paste0(jobs$info$school$table, 
                             jobs$info$school$suffix)
      school.df <<- database$ReadTable(school.table)
      
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        if (TODO || reworkjobs || reworkall) {
          
          # read input, process data table
          input.table.name <- job$input$table
          input.table.suffix <- job$input$suffix
          input.table <- paste0(job$input$table, job$input$suffix)
          in.df <<- database$ReadTable(input.table)
          
          process.table <- paste0(job$process$table, job$process$suffix)
          process.df <- database$ReadTable(process.table)
          process.df <- process.df[
            process.df[, kColumnTableName] == input.table.name, ]
          process.df <- process.df[
            process.df[, kColumnTableSuffix] == input.table.suffix, ]
          
          output.table <<- paste0(job$output$table, job$output$suffix)
          if (dropdata) {
            database$RemoveTable(output.table)
          }
          SQL <- paste("CREATE TABLE IF NOT EXISTS", output.table, DDL)
          
          database$CreateTable(output.table, SQL)
          
          LogInfo(paste("Score Converge", input.table, "through", process.table,
                        "into", output.table))
          print(process.df)
          k <- 1
          while (k <= nrow(process.df)) {
            process <<- process.df[k, ]
            stat.df <<- data.frame()
            stat.list <<- list()
            ProcessJob()
            k <- k + 1
          }
          
        }
        
      }
      
    }
    
  )
)