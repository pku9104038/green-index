## CleanData class for green index data processing and report generation


## flag for source loaded checking
cleandata.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexCleanData <- setRefClass(
  "GreenIndexCleanData",
  contains = "GreenIndexBase",
  
  fields = list(
    database = "GreenIndexDatabase"
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
    },
    
    CleanData = function(){
      
      # get job configuration
      jobs <- config$GetCleanDataJob()
      for (i in 1:length(jobs)){
        job <- jobs[[i]]
        TODO <- job$TODO
        if (TODO || config$IsReworkAll()) {
          
          
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
          
          LogInfo(paste("Checking", input.table))
          keys <- colnames(df)
          for (i in 1:length(keys)) {
            key <- keys[i][1]
            
            if (is.element(key, key.set)) {
              input <- unique(df[, key])
              input <- setdiff(input, kInvalidSet)
              
              choice.set <- choice.df[choice.df[, choice.key] == key, ]
              choice.set <- choice.set[, choice.value]
              
              if (!all(input %in% choice.set)) {
                # error founded 
                #LogWarn("Expected input:")
                #print(choice.set)
                #LogError(paste("Error input at:", key))
                #print(input)
                
                response.group  <- levels(factor(df[, key]))
                for(j in 1:length(response.group)){
                  group <- response.group[j]
                  response <- setdiff(group, kInvalidSet)
                  
                  if(length(response) > 0){
                    if (!all(response %in% choice.set)) {
                      LogWarn("Expected response:")
                      print(unlist(choice.set))
                      
                      LogError(paste("Error response at:", key))
                      print(response)
                      
                      
                      k <- 1
                      while(k <= length(recovery)){
                        if (recovery[[k]]$column == key){
                          response.err <- response
                          response <- sub(paste0("^", recovery[[k]]$error, "$"), 
                                          recovery[[k]]$fixed, response)
                          
                          if (!all(response == response.err)){
                            LogDebug("fixed response:")
                            print(response)
                            df[df[, key] == group, key] <- response
                            # break
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
          
          output.table <- paste0(job$output$table, job$output$suffix)
          database$WriteTable(df, output.table)
        }
        
      }
      
    }
  )
)
