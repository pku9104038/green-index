## StatisticsData class for green index data processing and report generation


## flag for source loaded checking
statistics.loaded <- TRUE

## define GreenIndexXlsx
library(methods)
library(digest)

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
    sample.df = "data.frame",
    stat.df = "data.frame",
    stat.list = "list",
    
    process = "data.frame",
    algorithm = "character",
    tier.name = "character",
    scope.name = "character",
    perspective.name = "character",
    sample.name = "character",
    variable.name = "character",
    choice.name = "character",
    statistics.value = "numeric",
    
    pilotrun = "character",
    pilot = "list"
    
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
    },
    
    InitStatList = function() {
      
      
      stat.list[kColumnHashDigest] <<- kHashDigestDefault
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
      stat.list[kColumnStatisticsPerspective] <<- ""
      stat.list[kColumnStatisticsSample] <<- ""
      stat.list[kColumnStatisticsVariable] <<- ""
      stat.list[kColumnStatisticsAlgorithm] <<- process[1, kColumnStatisticsAlgorithm]
      stat.list[kColumnValueType] <<- process[1, kColumnValueType]
      stat.list[kColumnKey] <<- ""
      stat.list[kColumnValue] <<- NA
      
    },
    
    HashStatList = function() {
      msg <- paste(stat.list[kColumnAssessment],
                   stat.list[kColumnGrade],
                   stat.list[kColumnSubject],
                   stat.list[kColumnDomain],
                   stat.list[kColumnDimention],
                   stat.list[kColumnGroup],
                   stat.list[kColumnAttribute],
                   stat.list[kColumnTopic],
                   stat.list[kColumnStatisticsTier],
                   stat.list[kColumnStatisticsScope],
                   stat.list[kColumnStatisticsPerspective],
                   stat.list[kColumnStatisticsSample],
                   stat.list[kColumnStatisticsVariable],
                   stat.list[kColumnStatisticsAlgorithm],
                   stat.list[kColumnValueType],
                   stat.list[kColumnKey])
      stat.list[kColumnHashDigest] <<- digest(msg, algo = "sha256")
      
    },
    
    AddStatDataframe = function() {
      
      stat.list[kColumnStatisticsTier] <<- tier.name
      stat.list[kColumnStatisticsScope] <<- scope.name
      stat.list[kColumnStatisticsPerspective] <<- perspective.name
      stat.list[kColumnStatisticsSample] <<- sample.name
      stat.list[kColumnStatisticsVariable] <<- variable.name
      stat.list[kColumnKey] <<- choice.name
      stat.list[kColumnValue] <<- statistics.value
      
      if (is.na(stat.list[kColumnKey]) || is.na(stat.list[kColumnValue][[1]])) {
        LogError(paste("AddStat",
                       stat.list[kColumnStatisticsAlgorithm], 
                       stat.list[kColumnStatisticsVariable], 
                       stat.list[kColumnDomain],
                       stat.list[kColumnDimention],
                       stat.list[kColumnGroup],
                       stat.list[kColumnAttribute],
                       stat.list[kColumnTopic],
                       stat.list[kColumnStatisticsTier], 
                       stat.list[kColumnStatisticsPerspective],
                       stat.list[kColumnStatisticsScope], 
                       stat.list[kColumnStatisticsSample], 
                       stat.list[kColumnKey],
                       stat.list[kColumnValue]))
      } else {
        HashStatList()
        stat.df <<- rbind(stat.df, data.frame(stat.list))
      }
      
    },
    
    InsertStatDataframe = function(){
      
      i <- 1
      while (i <= nrow(stat.df)) {
        
        LogDebug(paste("Insert",
                       stat.df[i, kColumnStatisticsTier], 
                       stat.df[i, kColumnStatisticsPerspective],
                       stat.df[i, kColumnStatisticsScope], 
                       stat.df[i, kColumnStatisticsSample], 
                       stat.df[i, kColumnStatisticsAlgorithm], 
                       stat.df[i, kColumnStatisticsVariable], 
                       stat.df[i, kColumnKey],
                       stat.df[i, kColumnValue]))
        
        df <- out.df[out.df[, kColumnHashDigest] == stat.df[i, kColumnHashDigest], ]
        
        if (nrow(df) == 0) {
          out.df <<- rbind(out.df, stat.df[i, ])
        } else {
          
          out.df[out.df[, kColumnHashDigest] == stat.df[i, kColumnHashDigest],
                 kColumnValue] <<- stat.df[i, kColumnValue] 
          
        }
        i <- i + 1
      }
      
    },

    SingleChoicePercent = function() {
      
      sample.size <- nrow(sample.df)
      
      choice.set <- unique(choice.df[choice.df[, choice.code] == 
                                       variable.name, choice.key])
      
      for (n in 1:length(choice.set)) {
        choice.name <<- choice.set[n]
        
        choice.size <- nrow(sample.df[sample.df[, variable.name] == choice.name, ])
        statistics.value <<- choice.size / sample.size * 100.0
        
        LogDebug(paste(algorithm, variable.name, 
                       kColumnStatisticsTier, tier.name, 
                       kColumnStatisticsPerspective, perspective.name,
                       kColumnStatisticsScope, scope.name, 
                       kColumnStatisticsSample, sample.name, 
                       choice.name, statistics.value))
        
        AddStatDataframe()    
      }
    },
    
    ValueSpacePercent = function() {
      
      sample.size <- nrow(sample.df)
      
      choice.set <- unique(sample.df[, variable.name])
      
      for (n in 1:length(choice.set)) {
        choice.name <<- choice.set[n]
        
        choice.size <- nrow(sample.df[sample.df[, variable.name] == choice.name, ])
        statistics.value <<- choice.size / sample.size * 100.0
        
        LogDebug(paste(algorithm, variable.name, 
                       kColumnStatisticsTier, tier.name, 
                       kColumnStatisticsPerspective, perspective.name,
                       kColumnStatisticsScope, scope.name, 
                       kColumnStatisticsSample, sample.name, 
                       choice.name, statistics.value))

        AddStatDataframe()    
      }
    },
    
    MultipleChoicePercent = function() {
      
      sample.size <- nrow(sample.df)
      
      variable.list <- unlist(strsplit(variable.name, kColumnMultipleChoice))
      i <- strtoi(variable.list[length(variable.list)])
      
      choice.set <- unique(choice.df[choice.df[, choice.code] == 
                                       variable.list[1], choice.key])
      
      choice.name <<- choice.set[i]
        
      choice.size <- nrow(sample.df[sample.df[, variable.name] == 1, ])
      statistics.value <<- choice.size / sample.size * 100.0
        
      LogDebug(paste(algorithm, variable.name, 
                     kColumnStatisticsTier, tier.name, 
                     kColumnStatisticsPerspective, perspective.name,
                     kColumnStatisticsScope, scope.name, 
                     kColumnStatisticsSample, sample.name, 
                     choice.name, statistics.value))
        
      AddStatDataframe()    

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
                      process[1, kColumnVariableName],
                      process[1, kColumnFilterName],
                      process[1, kColumnStatisticsAlgorithm]))
        
        InitStatList()
        
        filter.df <<- FilteringDataframe(in.df, 
                                         process[1, kColumnFilterName], 
                                         process[1, kColumnFilterType], 
                                         process[1, kColumnFilterValue])
        
        algorithm <<- process[1, kColumnStatisticsAlgorithm]
        
        tiers <- process[1, kColumnStatisticsTier]
        tiers <- unlist(strsplit(tiers, kSeparator))
        
        # limit perspective for pilot run
        perspectives <- process[1, kColumnStatisticsPerspective]
        perspectives <- unlist(strsplit(perspectives, kSeparator))
        if (pilotrun == kPilotRun && length(pilot$perspective) > 0) {
          perspectives <- pilot$perspective
        } 
        
        
        variables <- process[1, kColumnVariableName]
        variables <- unlist(strsplit(variables, kSeparator))
        
        # loop of variable
        for (i in 1:length(variables)) {
          variable.name <<- paste0(variables[i], 
                                  process[1, kColumnVariableSuffix])
          
          # loop for tier
          for (j in 1:length(tiers)) {
            tier.name <<- tiers[j]
            
            # loop for pespective
            for (k in 1:length(perspectives)) {
              perspective.name <<- perspectives[k]
              LogDebug(paste(algorithm, variable.name, 
                             kColumnStatisticsTier, tier.name, 
                             kColumnStatisticsPerspective, perspective.name))
              perspective.df <- filter.df[, c(variable.name, tier.name, 
                                              perspective.name)]
              
              
              # limit scope for pilot run
              scopes <- unique(perspective.df[, tier.name])
              if (pilotrun == kPilotRun && length(pilot$sample) > 0 && 
                  perspective.name == kPerspectiveTotal) {
                if (tier.name == kTierCity){
                  scopes <- pilot$sample$city
                } else if (tier.name == kTierDistrict){
                  scopes <- pilot$sample$district
                } else if (tier.name == kTierSchool){
                  scopes <- pilot$sample$school
                }
              } 
              # loop for scope
              for (l in 1:length(scopes)) {
                scope.name <<- scopes[l] 
                scope.df <- perspective.df[perspective.df[, tier.name] == 
                                             scope.name, ]
                
                # loop for sample
                samples <- unique(scope.df[, perspective.name])
                for (m in 1:length(samples)) {
                  sample.name <<- samples[m]
                  if (is.na(sample.name))
                    break
                  sample.df <<- scope.df[scope.df[, perspective.name] == 
                                           sample.name, ]
                  
                  if (algorithm == kAlgorithmSingleChoicePercent) {
                    SingleChoicePercent()
                  } else if (algorithm == kAlgorithmValueSpacePercent) {
                    ValueSpacePercent()
                  } else if (algorithm == kAlgorithmMultipleChoicePercent) {
                    MultipleChoicePercent()
                  } 
                  
                }
              }
              
            }
          }
        }
        
        InsertStatDataframe()
      }
    },
    
    StatisticsData = function(){
      LogInfo("Statistics Data!")
      
      # get job configuration
      jobs <- config$GetConfigJob()$statistics
      reworkall <- config$IsReworkAll()
      reworkjobs <- jobs$TODO
      pilotrun <<- jobs$RUN
      pilot <<- jobs$pilot
      
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
