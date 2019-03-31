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
    variable.type = "character"
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
      
      columns <- unlist(strsplit(transform[kColumnColumnName][[1]],"┋"))
      suffix <- transform[kColumnColumnSuffix] 
      df[, variable.name] <<- 0
      
      i <- 1
      while (i <= length(columns)){
        column.name <- paste0(columns[i], suffix)
        LogDebug(column.name)
        df[, variable.name] <<- df[, variable.name] + df[, column.name]
        i <- i + 1
      }
      df[, variable.name] <<- df[, variable.name] > parameter
      SetVariableType()
            
    },
    
    Constant = function(){
      
      if (parameter == "TRUE"){
        df[, variable.name] <<- TRUE
      } else {
        df[, variable.name] <<- parameter
      }
      SetVariableType()
      
    },
    
    AssignmentSigmaBinary = function(){
      
      columns <- unlist(strsplit(transform[kColumnColumnName][[1]],"┋"))
      suffix <- transform[kColumnColumnSuffix] 
      df[, variable.name] <<- 0
      
      i <- 1
      while (i <= length(columns)){
        column.name <- paste0(columns[i], suffix)
        LogDebug(column.name)
        df[, variable.name] <<- df[, variable.name] + df[, column.name]
        i <- i + 1
      }
      df[, variable.name] <<- df[, variable.name] > parameter
      SetVariableType()
      
    },
    
    TransformJob = function(){
      if (transform[kColumnTODO][[1]] == "FALSE"){
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
        } else if (algorithm == kAlgorithmAssignmentSigmaBinary) {
          AssignmentSigmaBinary()
        }
      }

    },
    
    
    TransformData = function(){
      LogInfo("Transform Data!")
      
      # get job configuration
      jobs <- config$GetTransformJob()
      reworkall <- config$IsReworkAll()
      reworkjobs <- jobs$TODO
      
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        
        if (TODO || reworkjobs || reworkall) {
          
          # read input, choice data table
          input.table.name <- job$input$table
          input.table.suffix <- job$input$suffix
          input.table <- paste0(job$input$table, job$input$suffix)
          df <<- database$ReadTable(input.table)
          
          transform.table <- paste0(job$transform$table, job$transform$suffix)
          transform.df <- database$ReadTable(transform.table)
          transform.df <- transform.df[
            transform.df[, kColumnTableName] == input.table.name, 
          ]
          transform.df <- transform.df[
            transform.df[, kColumnTableSuffix] == input.table.suffix,
          ]
          
          output.table <- paste0(job$output$table, job$output$suffix)
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
  )
)
