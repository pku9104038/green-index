## StatisticsData class for green index data processing and report generation


## flag for source loaded checking
statistics.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexStatisticsData <- setRefClass(
  "GreenIndexStatisticsData",
  contains = "GreenIndexBase",
  
  fields = list(
    database = "GreenIndexDatabase",
    in.df = "data.frame",
    out.df = "data.frame",
    filter.df = "data.frame",
    choice.df = "data.frame",
    choice.code = "character",
    choice.key = "character",
    stat.df = "data.frame",
    stat.list = "list",
    process = "data.frame",
    tiers = "character",
    algorithm = "character"
    
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
    },
    
    InsertStatDataframe = function(){
      LogDebug("InsertStatDataframe")
      df <- data.frame()
      for (i in 1:nrow(stat.df)) {
        df <- out.df[out.df[, kColumnAssessment] == stat.df[i, kColumnAssessment] &
                       out.df[, kColumnGrade] == stat.df[i, kColumnGrade] & 
                       out.df[, kColumnSubject] == stat.df[i, kColumnSubject] & 
                       out.df[, kColumnDomain] == stat.df[i, kColumnDomain] & 
                       out.df[, kColumnDimention] == stat.df[i, kColumnDimention] & 
                       out.df[, kColumnGroup] == stat.df[i, kColumnGroup] & 
                       out.df[, kColumnAttribute] == stat.df[i, kColumnAttribute] & 
                       out.df[, kColumnTopic] == stat.df[i, kColumnTopic] & 
                       out.df[, kColumnStatisticsTier] == stat.df[i, kColumnStatisticsTier] & 
                       out.df[, kColumnStatisticsScope] == stat.df[i, kColumnStatisticsScope] &
                       out.df[, kColumnStatisticsPerpective] == stat.df[i, kColumnStatisticsPerpective] & 
                       out.df[, kColumnStatisticsSample] == stat.df[i, kColumnStatisticsSample] & 
                       out.df[, kColumnStatisticsVariable] == stat.df[i, kColumnStatisticsVariable] & 
                       out.df[, kColumnStatisticsAlgorithm] == stat.df[i, kColumnStatisticsAlgorithm] &
                       out.df[, kColumnValueType] == stat.df[i, kColumnValueType] & 
                       out.df[, kColumnKey] == stat.df[i, kColumnKey], ]
        
        if (nrow(df) == 0) {
          out.df <<- rbind(out.df, stat.df[i, ])
        } else {
          
          out.df[out.df[, kColumnAssessment] == stat.df[i, kColumnAssessment] &
                   out.df[, kColumnGrade] == stat.df[i, kColumnGrade] & 
                   out.df[, kColumnSubject] == stat.df[i, kColumnSubject] & 
                   out.df[, kColumnDomain] == stat.df[i, kColumnDomain] & 
                   out.df[, kColumnDimention] == stat.df[i, kColumnDimention] & 
                   out.df[, kColumnGroup] == stat.df[i, kColumnGroup] & 
                   out.df[, kColumnAttribute] == stat.df[i, kColumnAttribute] & 
                   out.df[, kColumnTopic] == stat.df[i, kColumnTopic] & 
                   out.df[, kColumnStatisticsTier] == stat.df[i, kColumnStatisticsTier] & 
                   out.df[, kColumnStatisticsScope] == stat.df[i, kColumnStatisticsScope] &
                   out.df[, kColumnStatisticsPerpective] == stat.df[i, kColumnStatisticsPerpective] & 
                   out.df[, kColumnStatisticsSample] == stat.df[i, kColumnStatisticsSample] & 
                   out.df[, kColumnStatisticsVariable] == stat.df[i, kColumnStatisticsVariable] & 
                   out.df[, kColumnStatisticsAlgorithm] == stat.df[i, kColumnStatisticsAlgorithm] &
                   out.df[, kColumnValueType] == stat.df[i, kColumnValueType] & 
                   out.df[, kColumnKey] == stat.df[i, kColumnKey],  
                 kColumnValue] <<- stat.df[i, kColumnValue] 
        }
        
      }
      
    },
    
    AddStatDataframe = function() {
      stat.df <<- rbind(stat.df, data.frame(stat.list))
    },
    
    InitStatList = function() {
      
      stat.list[kColumnAssessment] <<- config$GetAssessmentName()
      stat.list[kColumnGrade] <<- process[1, kColumnGrade]
      stat.list[kColumnSubject] <<- process[1, kColumnSubject]
      stat.list[kColumnDomain] <<- process[1, kColumnDomain]
      stat.list[kColumnDimention] <<- process[1, kColumnDimention]
      stat.list[kColumnGroup] <<- process[1, kColumnGroup]
      stat.list[kColumnAttribute] <<- process[1, kColumnAttribute]
      stat.list[kColumnTopic] <<- process[1, kColumnTopic]
      stat.list[kColumnStatisticsTier] <<- ""
      stat.list[kColumnStatisticsScope] <<- ""
      stat.list[kColumnStatisticsPerpective] <<- ""
      stat.list[kColumnStatisticsSample] <<- ""
      stat.list[kColumnStatisticsVariable] <<- ""
      stat.list[kColumnStatisticsAlgorithm] <<- process[1, kColumnStatisticsAlgorithm]
      stat.list[kColumnValueType] <<- process[1, kColumnValueType]
      stat.list[kColumnKey] <<- ""
      stat.list[kColumnValue] <<- NA
      
    },
    
    
    
    SingleChoicePercent = function() {
      LogDebug("SingleChoicePercent")
      InitStatList()
      
      tiers <- process[1, kColumnStatisticsTier]
      tiers <- unlist(strsplit(tiers, kSeparator))
      perspectives <- process[1, kColumnStatisticsPerpective]
      perspectives <- unlist(strsplit(perspectives, kSeparator))
      variables <- process[1, kColumnVariableName]
      variables <- unlist(strsplit(variables, kSeparator))
      
      # loop for pespective
      # variable
      for (i in 1:length(variables)) {
        variable.name <- paste0(variables[i], 
                                process[1, kColumnVariableSuffix])
        
        # tier
        
        for (j in 1:length(tiers)) {
          tier.name <- tiers[j]
          
          # pespective
          for (k in 1:length(perspectives)) {
            perspective.name <- perspectives[k]
            LogDebug(paste(algorithm, variable.name, 
                           kColumnStatisticsTier, tier.name, 
                           kColumnStatisticsPerpective, perspective.name))
            perspective.df <- filter.df[, c(variable.name, tier.name, perspective.name)]
            
            # loop for choice
            scopes <- unique(perspective.df[, tier.name])
            for (l in 1:length(scopes)) {
              scope.name <- scopes[l] 
              scope.df <- perspective.df[perspective.df[, tier.name] == scope.name, ]
              
              samples <- unique(scope.df[, perspective.name])
              for (m in 1:length(samples)) {
                sample.name <- samples[m]
                if (is.na(sample.name))
                  break
                sample.df <- scope.df[scope.df[, perspective.name] == sample.name, ]
                
                sample.size <- nrow(sample.df)
                
                #choice <- unique(df[, variable.name])
                choice.set <- unique(choice.df[choice.df[, choice.code] == 
                                               variable.name, choice.key])
                
                for (n in 1:length(choice.set)) {
                  choice.name <- choice.set[n]
                  
                  choice.size <- nrow(sample.df[sample.df[, variable.name] == choice.name, ])
                  statistics.value <- choice.size / sample.size * 100.0
                  
                  LogDebug(paste(algorithm, variable.name, 
                                 kColumnStatisticsTier, tier.name, 
                                 kColumnStatisticsPerpective, perspective.name,
                                 scope.name, sample.name, choice.name,
                                 statistics.value))
                  
                  stat.list[kColumnStatisticsTier] <<- tier.name
                  stat.list[kColumnStatisticsScope] <<- scope.name
                  stat.list[kColumnStatisticsPerpective] <<- perspective.name
                  stat.list[kColumnStatisticsSample] <<- sample.name
                  stat.list[kColumnStatisticsVariable] <<- variable.name
                  stat.list[kColumnKey] <<- choice.name
                  stat.list[kColumnValue] <<- statistics.value
                  
                  AddStatDataframe()
                }
              }
            }
            
          }
        }
      }
      
      InsertStatDataframe()
    },
    
    BooleanPercent = function() {
      LogDebug("BooleanPercent")
      InitStatList()
      
      tiers <- process[1, kColumnStatisticsTier]
      tiers <- unlist(strsplit(tiers, kSeparator))
      perspectives <- process[1, kColumnStatisticsPerpective]
      perspectives <- unlist(strsplit(perspectives, kSeparator))
      variables <- process[1, kColumnVariableName]
      variables <- unlist(strsplit(variables, kSeparator))
      
      # loop for pespective
      # variable
      for (i in 1:length(variables)) {
        variable.name <- paste0(variables[i], 
                                process[1, kColumnVariableSuffix])
        
        # tier
        
        for (j in 1:length(tiers)) {
          tier.name <- tiers[j]
          
          # pespective
          for (k in 1:length(perspectives)) {
            perspective.name <- perspectives[k]
            LogDebug(paste(algorithm, variable.name, 
                           kColumnStatisticsTier, tier.name, 
                           kColumnStatisticsPerpective, perspective.name))
            perspective.df <- filter.df[, c(variable.name, tier.name, perspective.name)]
            
            # loop for choice
            scopes <- unique(perspective.df[, tier.name])
            for (l in 1:length(scopes)) {
              scope.name <- scopes[l] 
              scope.df <- perspective.df[perspective.df[, tier.name] == scope.name, ]
              
              samples <- unique(scope.df[, perspective.name])
              for (m in 1:length(samples)) {
                sample.name <- samples[m]
                if (is.na(sample.name))
                  break
                sample.df <- scope.df[scope.df[, perspective.name] == sample.name, ]
                sample.size <- nrow(sample.df)
                
                
                # algorithm core
                choice.set <- unique(sample.df[, variable.name])
                # choice.set <- unique(choice.df[choice.df[, choice.code] == 
                #                                 variable.name, choice.key])
                
                for (n in 1:length(choice.set)) {
                  choice.name <- choice.set[n]
                  if (is.na(choice.name))
                    break
                  choice.size <- nrow(sample.df[sample.df[, variable.name] == choice.name, ])
                  statistics.value <- choice.size / sample.size * 100.0
                  
                  
                  
                  LogDebug(paste(algorithm, variable.name, 
                                 kColumnStatisticsTier, tier.name, 
                                 kColumnStatisticsPerpective, perspective.name,
                                 scope.name, sample.name, choice.name,
                                 statistics.value))
                  
                  stat.list[kColumnStatisticsTier] <<- tier.name
                  stat.list[kColumnStatisticsScope] <<- scope.name
                  stat.list[kColumnStatisticsPerpective] <<- perspective.name
                  stat.list[kColumnStatisticsSample] <<- sample.name
                  stat.list[kColumnStatisticsVariable] <<- variable.name
                  stat.list[kColumnKey] <<- choice.name
                  stat.list[kColumnValue] <<- statistics.value
                  
                  
                  
                  AddStatDataframe()
                }
              }
            }
            
          }
        }
      }
      
      InsertStatDataframe()
    },
    
    ProcessJob = function(){
      
      if (process[1, kColumnTODO] == "FALSE"){
        return(NA)
        
      } else if (process[1, kColumnTODO] == "TRUE"){
        
        filter.df <<- FilteringDataframe(in.df, 
                                        process[1, kColumnFilterName], 
                                        process[1, kColumnFilterType], 
                                        process[1, kColumnFilterValue])
        
        algorithm <<- process[1, kColumnStatisticsAlgorithm]
        if (algorithm == kAlgorithmSingleChoicePercent) {
          SingleChoicePercent()
        } else if (algorithm == kAlgorithmBooleanPercent) {
          BooleanPercent()
        } 
        
      }
    },
    
    
    StatisticsData = function(){
      LogInfo("Statistics Data!")
      
      # get job configuration
      jobs <- config$GetConfigJob()$statistics
      reworkall <- config$IsReworkAll()
      reworkjobs <- jobs$TODO
      #tiers <<- jobs$tier
      
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
          
          # merge join data for more attributes
          join.table.name <- job$join$table
          join.table.suffix <- job$join$suffix
          join.table <- paste0(join.table.name, join.table.suffix)
          join.df <- database$ReadTable(join.table)
          join.by <- job$join$by
          
          # in.df <<- merge(in.df, join.df, by.x = join.by, by.y = join.by,
          #                all = FALSE, all.x = TRUE, all.y = FALSE)
          
          process.table <- paste0(job$process$table, job$process$suffix)
          process.df <- database$ReadTable(process.table)
          process.df <- process.df[
            process.df[, kColumnTableName] == input.table.name, 
            ]
          process.df <- process.df[
            process.df[, kColumnTableSuffix] == input.table.suffix,
            ]
          
          choice.table <- paste0(job$choice$table, job$choice$suffix)
          choice.df <<- database$ReadTable(choice.table)
          choice.code <<- job$choice$column$code
          choice.key <<- job$choice$column$key
          choice.df <<- choice.df[, c(choice.code, choice.key)]
          
          
          output.table <- paste0(job$output$table, job$output$suffix)
          out.df <<- database$ReadTable(output.table)
          
          LogInfo(paste("Statistics", input.table, "through", process.table,
                        "into", output.table))
          
          k <- 1
          while (k <= nrow(process.df)) {
            process <<- process.df[k, ]
            stat.df <<- data.frame()
            stat.list <<- list()
            ProcessJob()
            k <- k + 1
          }
          
          database$WriteTable(out.df, output.table)
        }
        
      }
      
    }
  )
)
