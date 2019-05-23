## SplitData class for green index data processing and report generation


## flag for source loaded checking
splitdata.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexSplitData <- setRefClass(
  "GreenIndexSplitData",
  contains = "GreenIndexBase",
  
  fields = list(
    database = "GreenIndexDatabase"
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
    },
    
    SplitData = function(){
      LogInfo("Spliting Data!")
      # get job configuration
      jobs <- config$GetSplitDataJob()
      reworkall <- config$IsReworkAll()
      reworkjobs <- jobs$TODO
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        if (TODO || reworkjobs || reworkall) {
          
          # read table.in, table.choice data table
          input.table <- paste0(job$input$table, job$input$suffix)
          column.id <- job$input$column$id
          df <- database$ReadTable(input.table)
          
          choice.table <- paste0(job$choice$table, job$choice$suffix)
          choice.df <- database$ReadTable(choice.table)
          choice.key <- job$choice$column$key
          choice.value <- job$choice$column$value
          choice.df <- choice.df[, c(choice.key, choice.value)]
          key.set <- unique(choice.df[,choice.key])
          
          output.table <- paste0(job$output$table, job$output$suffix)
          LogInfo(paste("Split", input.table, "multiple choice column into", 
                        output.table))
          
          columns <- job$column
          separator <- job$separator
          for (i in 1:length(columns)) {
            column <- columns[[i]]$name
            recovery <- columns[[i]]$recovery
            LogInfo(paste("Processing column",column))
            choice.set <- choice.df[choice.df[, choice.key] == column, ]
            #print(choice.set)
            choice.set <- choice.set[, choice.value]
            #print(choice.set)
            for (j in 1:length(choice.set)) {
              column.name <- paste0(column, kColumnMultipleChoice, 
                                    as.character(j))
              df[column.name] <- 0
              
              # column.var <- paste0(column, "_", choice.set[[j]])
              # df[column.var] <- 0
              # df[df[, column] == "", column.var] <- NA
            }
            
            response.group  <- levels(factor(df[, column]))
            for(j in 1:length(response.group)){
              group <- response.group[j]
              response <- setdiff(group, kInvalidSet)
              response <- unlist(strsplit(response, kSeparator))
              
              if(length(response) > 0){
                if (!all(response %in% choice.set)) {
                  LogWarn(paste("Expect value in:"))
                  print(unlist(choice.set))
                  
                  LogError(paste("Error response:", column))
                  print(response)
                  print(df[df[, column] == group, column.id])
                  
                  k <- 1
                  while(k <= length(recovery)){
                    
                    response.err <- response
                    response <- sub(paste0("^", recovery[[k]]$error, "$"), 
                                    recovery[[k]]$fixed, response)
                    
                    if (!all(response == response.err)){
                      LogDebug("Change value to:")
                      print(response)
                      break
                    }
                    k = k + 1
                    
                  }
                  
                }
                
                #response <- unlist(response)
                for(k in 1:length(choice.set)){
                  if(is.element(choice.set[[k]], response)){
                    column.name <- paste0(column, kColumnMultipleChoice, 
                                          as.character(k))
                    
                    df[df[, column] == group, column.name] <- 1  # choice.set[[k]]
                    
                    # column.var <- paste0(column, "_", choice.set[[k]])
                    # df[df[, column] == group, column.var] <- 1
                  }
                }
              }
            }
            
            # names(df) <- sub(paste0("^", column, "$"), 
            #                 paste0(column, kColumnMultipleChoice, 
            #                        as.character(0)), 
            #                 names(df))
            
          }
          database$WriteTable(df, output.table)
          
        }
        
      }
      
    }
  )
)
