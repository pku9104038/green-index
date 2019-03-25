## CheckData class for green index data processing and report generation


## flag for source loaded checking
checkdata.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexCheckData <- setRefClass(
  "GreenIndexCheckData",
  contains = "GreenIndexBase",
  
  fields = list(
    database = "GreenIndexDatabase"
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
    },
    
    CheckData = function(){
      LogInfo("Checking Data!")
      
      # get job configuration
      jobs <- config$GetCheckDataJob()
      reworkall <- config$IsReworkAll()
      reworkjobs <- jobs$TODO
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        if (TODO || reworkjobs || reworkall) {
          
          
          # read input, choice data table
          input.table <- paste0(job$input$table, job$input$suffix)
          df <- database$ReadTable(input.table)
          
          choice.table <- paste0(job$choice$table, job$choice$suffix)
          choice.df <- database$ReadTable(choice.table)
          choice.key <- job$choice$column$key
          choice.value <- job$choice$column$value
          choice.df <- choice.df[, c(choice.key, choice.value)]
          key.set <- unique(choice.df[,choice.key])
          
          recovery <- job$recovery
          output.table <- paste0(job$output$table, job$output$suffix)
          LogInfo(paste("Filtering", input.table, "throught", choice.table,
                        "into", output.table))
          
          keys <- colnames(df)
          for (i in 1:length(keys)) {
            key <- keys[i][1]
            
            # find key in key.set
            if (is.element(key, key.set)) {
              # all input without invalid set
              input <- unique(df[, key])
              input <- setdiff(input, kInvalidSet)
              
              # available choice.set
              choice.set <- choice.df[choice.df[, choice.key] == key, ]
              choice.set <- choice.set[, choice.value]
            
              # check input inside choice.set
              if (!all(input %in% choice.set)) {
               
                response.group  <- levels(factor(df[, key]))
                print(response.group)
                for(j in 1:length(response.group)){
                  group <- response.group[j]
                  response <- setdiff(group, kSkipSet)
                  LogDebug(paste("checking response group",group))
                  
                  if(length(response) > 0){
                    if (!all(response %in% choice.set)) {
                      LogWarn(paste("Expect value in:"))
                      print(unlist(choice.set))
                      
                      LogError(paste("Error response:", key))
                      print(response)
                      
                      
                      k <- 1
                      while(k <= length(recovery)){
                        if (recovery[[k]]$column == key){
                          response.err <- response
                          response <- sub(paste0("^", recovery[[k]]$error, "$"), 
                                          recovery[[k]]$fixed, response)
                          
                          if (!all(response == response.err)){
                            df[df[, key] == group, key] <- response
                            LogDebug("Change value to:")
                            print(response)
                          }
                          
                        }
                        k = k + 1
                      }
                      
                    }
                    
                  }
                }
                 
              }
                
            }
          }
          
          database$WriteTable(df, output.table)
        }
        
      }
      
    }
  )
)
