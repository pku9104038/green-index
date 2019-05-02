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
      df[, variable.name] <<- kFALSE
      df[df[, variable.value.name] > as.numeric(parameter), variable.name] <<- kTRUE
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
        }
        LogDebug(column.name)
        
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
      
      average.value <- mean(df[, transform[kColumnColumnName][[1]]], na.rm = TRUE)
      stdev.value <- sd(df[, transform[kColumnColumnName][[1]]], na.rm = TRUE)
      df[, variable.name] <<- average.point + 
        (df[, transform[kColumnColumnName][[1]]] - average.value) / stdev.value * z.point
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
