## AssignPoint class for green index data processing and report generation


## flag for source loaded checking
assignpoint.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexAssignPoint <- setRefClass(
  "GreenIndexAssignPoint",
  contains = "GreenIndexBase",
  
  fields = list(
    database = "GreenIndexDatabase"
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
    },
    
    AssignPoint = function(){
      LogInfo("Assign Points!")
      
      # get job configuration
      jobs <- config$GetAssignPointJob()
      reworkall <- config$IsReworkAll()
      dropdata <- config$IsDropData()
      reworkjobs <- jobs$TODO
      
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        suffix <- job$suffix
        
        if (TODO || reworkjobs || reworkall) {
          
          # read input, choice data table
          input.table <- paste0(job$input$table, job$input$suffix)
          output.table <- paste0(job$output$table, job$output$suffix)
          if (dropdata) {
            df <- database$ReadTable(input.table)
            
          } else {
            df <- database$ReadTable(output.table)
          }
          
          choice.table <- paste0(job$choice$table, job$choice$suffix)
          choice.df <- database$ReadTable(choice.table)
          choice.code <- job$choice$column$code
          choice.key <- job$choice$column$key
          choice.value <- job$choice$column$value
          choice.df <- choice.df[, c(choice.code, choice.key, 
                                     choice.value, kColumnTODO)]
          choice.df[, choice.value] <- as.numeric(choice.df[, choice.value])
          code.set <- unique(choice.df[, choice.code])
          
          LogInfo(paste("Assignment", input.table, "by", choice.table,
                        "into", output.table))
          
          codes <- colnames(df)
          for (i in 1:length(codes)) {
            code <- codes[i][1]
            
            # find code in code.set
            if (is.element(code, code.set)) {
              
              option <- choice.df[choice.df[, choice.code] == code, ]
              # skip FALSE row
              if (option[1, kColumnTODO] == kFALSE) {
                break
              }
              
              option <- option[c(choice.key, choice.value)]
              # make sure the option should be assign a point
              if (!is.na(option[1, choice.value])){
                
                option[nrow(option)+1 , choice.key] <- kNullStr
                option[nrow(option) , choice.value] <- NA
                
                column.assignment <- paste0(code, suffix)
                names(option) <- sub(paste0("^", choice.value, "$"), 
                                     column.assignment, names(option))
                
                LogDebug(column.assignment)
                
                df <- merge(df, option, by.x = code, by.y = choice.key,
                            all.x = TRUE)
                
                LogDebug(paste(column.assignment, 
                               as.character(mean(df[, column.assignment], 
                                                 na.rm = TRUE))))
                
              }
             
            }
          }
          
          database$WriteTable(df, output.table)
        }
        
      }
      
    }
  )
)
