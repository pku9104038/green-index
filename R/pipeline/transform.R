## TransformData class for green index data processing and report generation


## flag for source loaded checking
transform.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexTransformData <- setRefClass(
  "GreenIndexTransformData",
  contains = "GreenIndexBase",
  
  fields = list(
    database = "GreenIndexDatabase",
    
    attribute.df = "data.frame",
    df = "data.frame",
    transform = "data.frame",
    algorithm = "character",
    parameter = "character",
    variable.name = "character",
    variable.type = "character",
    
    RUN = 'character'
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
    },
    
    SetVariableType = function(){
      if (variable.type == kDataTypeCharacter){
        df[, variable.name] <<- as.character(df[, variable.name])
      } else if(variable.type == kDataTypeNumeric) {
        df[, variable.name] <<- as.numeric(df[, variable.name])
      } else if(variable.type == kDataTypeBoolean) {
        
      }
      
    },
    
    
    SigmaBinary = function(){
      
      columns <- unlist(strsplit(transform[kColumnColumnName][[1]], kSeparator))
      suffix <- transform[kColumnColumnSuffix] 
      variable.value.name <- paste0(variable.name, kColumnSuffixPoint)
      df[, variable.value.name] <<- 0
      
      i <- 1
      while (i <= length(columns)){
        column.name <- paste0(columns[i], suffix)
        LogDebug(column.name)
        df[, variable.value.name] <<- df[, variable.value.name] + df[, column.name]
        i <- i + 1
      }
      df[is.na(df[, variable.value.name]), variable.value.name] <<- kNumericNA
      
      param <- unlist(strsplit(parameter, kSeparator))
      threshold <- as.numeric(param[1])
      if (length(param) == 1){
        df[, variable.name] <<- kFALSE
        df[df[, variable.value.name] > threshold, variable.name] <<- kTRUE
      } else if (length(param) == 2) {
        value <- as.numeric(param[2])
        df[, variable.name] <<- 0
        df[df[, variable.value.name] > threshold, variable.name] <<- value
      }
      
      # df[, variable.value.name] <<- NULL
      SetVariableType()
            
    },
    
    SigmaValue = function(){
      
      columns <- unlist(strsplit(transform[kColumnColumnName][[1]], kSeparator))
      suffix <- transform[kColumnColumnSuffix] 
      df[, variable.name] <<- 0
      
      i <- 1
      while (i <= length(columns)){
        column.name <- paste0(columns[i], suffix)
        LogDebug(column.name)
        df[, variable.name] <<- df[, variable.name] + 
          as.numeric(df[, column.name])
        i <- i + 1
      }
      
      if (max(df[, variable.name]) > as.numeric(parameter)) {
        LogError(paste(variable.name, algorithm))
      }
      
      SetVariableType()
      
    },
    
    CleanChoice = function() {
      
      attr.df <- attribute.df[attribute.df[, kColumnSubject] == 
                                transform[kColumnTableName][[1]],]
      columns <- attr.df[attr.df[, kColumnQuestionType] == 
                           kQuestionTypeSingleChoice, kColumnQuestionCode]
      columns <- unlist(columns)
      
      parameter.vector <- unlist(strsplit(parameter, kSeparator))
      space.char <- parameter.vector[1]
      value.set <- unlist(strsplit(
        parameter.vector[length(parameter.vector)], kSegmentConnector))
      parameter.set <- parameter.vector[2:length(parameter.vector)-1]
      print(parameter.set)
      print(parameter.vector)
      print(value.set)
      column.suffix <- transform[kColumnColumnSuffix] 
      variable.suffix <- transform[kColumnVariableSuffix]
      i <- 1
      while (i <= length(columns)){
        column.name <- paste0(columns[i], column.suffix)
        variable.name <<- paste0(columns[i], variable.suffix)
        
        df[,"tmp"] <<-  df[, column.name] %in% parameter.set
        df[df[, "tmp"] == FALSE, variable.name] <<- value.set[2]
        
        for (j in 1:length(parameter.set)) {
          df[df[, column.name] == parameter.set[j], variable.name] <<- 
            parameter.set[j]
        }
        df[df[, column.name] == space.char, variable.name] <<- value.set[1]
        i <- i + 1
      }
      
      SetVariableType()      
    },
    
    GreatThanMean = function() {
      
      column.name <- paste0(transform[kColumnColumnName][[1]], 
                            transform[kColumnColumnSuffix][[1]])
      print(column.name)
      threshold <- mean(as.numeric(df[, column.name]))
      print(threshold)
      value.set <- unlist(strsplit(parameter, kSeparator))
      print(value.set)
      df[df[, column.name] > threshold, variable.name] <<- value.set[1] 
      df[df[, column.name] <= threshold, variable.name] <<- value.set[2] 
      
    },
    
    GroupSigma = function() {
      
      attr.df <- attribute.df[attribute.df[, kColumnSubject] == 
                                transform[kColumnTableName][[1]],]
      
      attr <- transform[kColumnParameter][[1]]
      var <- transform[kColumnVariableName][[1]]
      attr.df <- attr.df[attr.df[, attr ] == var, ]
      columns <- attr.df[, kColumnQuestionCode]
      columns <- unlist(columns)
      
      value.max <- sum(as.numeric(attr.df[, kColumnPointValue]))
      # columns <- unlist(strsplit(transform[kColumnColumnName][[1]], kSeparator))
      suffix <- transform[kColumnColumnSuffix] 
      df[, variable.name] <<- 0
      
      i <- 1
      while (i <= length(columns)){
        column.name <- paste0(columns[i], suffix)
        LogDebug(column.name)
        df[, variable.name] <<- df[, variable.name] + 
          as.numeric(df[, column.name])
        i <- i + 1
      }
      
      if (max(df[, variable.name]) > value.max) {
        LogError(paste(variable.name, algorithm))
      }
      
      SetVariableType()      
    },
    
    SigmaTotalValue = function(){
      
      columns <- names(df)
      suffix <- transform[kColumnColumnSuffix] 
      # variable.name2 <- paste0(variable.name, "2")
      df[, variable.name] <<- 0
      
      i <- 1
      while (i <= length(columns)){
        column.name <- columns[i]
        if (substr(column.name, nchar(column.name)-1,
                   nchar(column.name)) == suffix) {
          df[, variable.name] <<- df[, variable.name] + 
            as.numeric(df[, column.name])
          LogDebug(column.name)
        }
        
        
        i <- i + 1
      }
      if (max(df[, variable.name]) > as.numeric(parameter)) {
        LogError(paste(variable.name, algorithm))
      }
      
      SetVariableType()
      
    },
    
    Standardization = function(){
    
      parameters <- unlist(strsplit(parameter, kSeparator))
      average.point <- as.numeric(parameters[1])
      z.point <- as.numeric(parameters[2])
      column.name <- paste0(transform[kColumnColumnName], transform[kColumnColumnSuffix])
      print(column.name)
      average.value <- mean(df[, column.name], na.rm = TRUE)
      stdev.value <- sd(df[, column.name], na.rm = TRUE)
      df[, variable.name] <<- average.point + 
        (df[, column.name] - average.value) / stdev.value * z.point
      df[df[, variable.name] < 0, variable.name] <<- 0
      
      SetVariableType()
      
    },
    
    ScoreSegment = function(){
      
      columns <- unlist(strsplit(transform[kColumnColumnName][[1]], kSeparator))
      suffix <- transform[kColumnColumnSuffix] 
      df[, variable.name] <<- ""
      
      score.segments <- unlist(strsplit(parameter, kSeparator))
      i <- 1
      while (i <= length(columns)){
        column.name <- paste0(columns[i], suffix)
        kSegmentConnector
        for (j in 1:length(score.segments)) {
          score.segment <- unlist(strsplit(score.segments[j], kSegmentConnector))
          min <- as.numeric(score.segment[1])
          max <- as.numeric(score.segment[2])
          df[df[, column.name] >= min & df[, column.name] < max, 
             variable.name] <<- score.segments[j]
        }
        i <- i + 1
      }
      
      SetVariableType()
      
    },
    
    ScoreRank = function(){
      columns <- unlist(strsplit(transform[kColumnColumnName][[1]], kSeparator))
      suffix <- transform[kColumnColumnSuffix] 
      df[, variable.name] <<- ""
      
      score.segments <- unlist(strsplit(parameter, kSeparator))
      i <- 1
      while (i <= length(columns)){
        column.name <- paste0(columns[i], suffix)
        for (j in 1:length(score.segments)) {
          score.segment <- unlist(strsplit(score.segments[j], kSegmentConnector))
          rank <- score.segment[1]
          min <- as.numeric(score.segment[2])
          max <- as.numeric(score.segment[3])
          df[df[, column.name] >= min & df[, column.name] < max, 
             variable.name] <<- rank
        }
        i <- i + 1
      }
      
      SetVariableType()
      
    },
    
    ValueMapping = function(){
      
      columns <- unlist(strsplit(transform[kColumnColumnName][[1]], kSeparator))
      suffix <- transform[kColumnColumnSuffix] 
      df[, variable.name] <<- ""
      
      value.maps <- unlist(strsplit(parameter, kSeparator))
      i <- 1
      while (i <= length(columns)){
        column.name <- paste0(columns[i], suffix)
        for (j in 1:length(value.maps)) {
          value.map <- unlist(strsplit(value.maps[j], kSegmentConnector))
          map <- value.map[1]
          for (k in 2:length(value.map)) {
            value <- value.map[k]
            df[df[, column.name] == value, variable.name] <<- map
          }
          
        }
        i <- i + 1
      }
      
      SetVariableType()
      
    },
    
    Constant = function(){
      
      if (parameter == "TRUE"){
        df[, variable.name] <<- kTRUE
      } else {
        df[, variable.name] <<- parameter
      }
      SetVariableType()
      
    },
    
    
    TransformJob = function(){
      if (transform[kColumnTODO][[1]] == "FALSE" &&  RUN == kPilotRun){
        return(NA)
      } else if (transform[kColumnTODO][[1]] == "TRUE"){
        
        variable.name <<- paste0(transform[kColumnVariableName],
                                 transform[kColumnVariableSuffix])
        variable.type <<- transform[kColumnVariableType][[1]]
        algorithm <<- transform[kColumnAlgorithm][[1]]
        parameter <<- transform[kColumnParameter][[1]]
        LogInfo(paste("algorithm", algorithm, "variable", variable.name,
                      "parameter", parameter))
        
        if (algorithm == kAlgorithmSigmaBinary) {
          SigmaBinary()
        } else if (algorithm == kAlgorithmConstant) {
          Constant()
        } else if (algorithm == kAlgorithmSigmaTotalScore) {
          SigmaTotalValue()
        } else if (algorithm == kAlgorithmSigmaValue) {
          SigmaValue()
        } else if (algorithm == kAlgorithmCleanChoice) {
          CleanChoice()
        } else if (algorithm == kAlgorithmGreatThanMean) {
          GreatThanMean()
        } else if (algorithm == kAlgorithmGroupSigma) {
          GroupSigma()
        } else if (algorithm == kAlgorithmStandardization) {
          Standardization()
        } else if (algorithm == kAlgorithmScoreSegment) {
          ScoreSegment()
        } else if (algorithm == kAlgorithmScoreRank) {
          ScoreRank()
        } else if (algorithm == kAlgorithmValueMapping) {
          ValueMapping()
        } 
      }

    },
    
    
    TransformData = function(){
      LogInfo("Transform Data!")
      
      # get job configuration
      jobs <- config$GetTransformJob()
      reworkall <- config$IsReworkAll()
      reworkjobs <- jobs$TODO
      RUN <<- jobs$RUN
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        
        if (TODO || reworkjobs || reworkall) {
          
          # read input, choice data table
          input.table.name <- job$input$table
          input.table.suffix <- job$input$suffix
          input.table <- paste0(job$input$table, job$input$suffix)
          df <<- database$ReadTable(input.table)
          
          if (!is.null(job$attribute)) {
            attribute.table <- paste0(job$attribute$table, job$attribute$suffix)
            attribute.df <<- database$ReadTable(attribute.table)  
          }
          
          output.table <- paste0(job$output$table, job$output$suffix)
          
          
          if (length(job$dummy) > 0) {
            # clone a dummy transform table
            LogInfo(paste("Clone", input.table, "into", output.table))
            database$WriteTable(df, output.table)
          } else {
            
            transform.table <- paste0(job$transform$table, job$transform$suffix)
            transform.df <- database$ReadTable(transform.table)
            transform.df <- transform.df[
              transform.df[, kColumnTableName] == input.table.name, 
              ]
            transform.df <- transform.df[
              transform.df[, kColumnTableSuffix] == input.table.suffix,
              ]
            
            transform.df <- arrange(transform.df, as.numeric(transform.df[, kColumnSN]))
            LogInfo(paste("Transform", input.table, "by", transform.table,
                          "into", output.table))
            
            k <- 1
            while (k <= nrow(transform.df)) {
              transform <<- transform.df[k, ]
              TransformJob()
              
              k <- k + 1
            }
            
            database$WriteTable(df, output.table)            
          }
         
        }
        
      }
      
    }
  )
)
