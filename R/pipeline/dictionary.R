## Dictionary class for green index data processing and report generation


## flag for source loaded checking
dictionary.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexDictionary <- setRefClass(
  "GreenIndexDictionary",
  contains = "GreenIndexBase",
  
  fields = list(
    database = "GreenIndexDatabase"
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
    },
    
    Dictionary = function(){
      LogInfo("Dictionary!")
      
      # get job configuration
      jobs <- config$GetDictionaryJob()
      reworkall <- config$IsReworkAll()
      reworkjobs <- jobs$TODO
      
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        suffix <- job$suffix
        
        if (TODO || reworkjobs || reworkall) {
          
          # read input, choice data table
          input.table <- paste0(job$input$table, job$input$suffix)
          df <- database$ReadTable(input.table)
          
          choice.table <- paste0(job$choice$table, job$choice$suffix)
          choice.df <- database$ReadTable(choice.table)
          choice.code <- job$choice$column$code
          choice.key <- job$choice$column$key
          choice.value <- job$choice$column$value
          choice.df <- choice.df[, c(choice.code, choice.key, choice.value)]
          #choice.df[, choice.value] <- as.numeric(choice.df[, choice.value])
          code.set <- unique(choice.df[, choice.code])
          
          
          output.table <- paste0(job$output$table, job$output$suffix)
          LogInfo(paste("Dictionary", input.table, "by", choice.table,
                        "into", output.table))
          
          codes <- colnames(df)
          for (i in 1:length(codes)) {
            code <- codes[i][1]
            
            # find code in code.set
            if (is.element(code, code.set)) {
              
              option <- choice.df[choice.df[, choice.code] == code, ]
              option <- option[c(choice.key, choice.value)]
              
              
              # make sure the option should be assign a point
              if (!is.na(option[1, choice.value])){
                
                option[nrow(option)+1 , choice.key] <- kNullStr
                option[nrow(option) , choice.value] <- NA
                
                #column.assignment <- paste0(code, "_name")
                column.assignment <- "tmp"
                names(option) <- sub(paste0("^", choice.value, "$"), 
                                     column.assignment, names(option))
                
                LogDebug(code)
          
                
                df <- merge(df, option, by.x = code, by.y = choice.key,
                            all.x = TRUE)
                
                df[, code] <- df[, "tmp"]
                df[, "tmp"] <- NULL
                
                
                
                
              }
              
            }
          }
          
          database$WriteTable(df, output.table)
        }
        
      }
      
    }
  )
)
