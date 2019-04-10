## GreenIndexBase, the parent of all green index class except GreenIndexConfig

## flag for source loaded checking
base.loaded <- TRUE


## difine basic class
GreenIndexBase <- setRefClass(
  "GreenIndexBase",
  
  fields = list(
    module = "character",
    config = "GreenIndexConfig"
    
  ),
  
  methods = list(
    
    # Log
    LogComposer = function(msg, ...){
      paste0(" <", module, "> ",msg)
    },
    LogInfo = function(msg, format = ""){
      loginfo(msg, format, logger = module)
    },
    LogWarn = function(msg, format = ""){
      logwarn(msg, format, logger = module)
    },
    LogDebug = function(msg, format = ""){
      logdebug(paste0(" ",msg), format, logger = module)
    },
    LogError = function(msg, format = ""){
      logerror(paste0(" ",msg), format, logger = module)
    },
    
    
    # Init 
    Init = function(module.name, config.obj){
      module <<- module.name
      config <<- config.obj
      loglevel <- config$GetLogLevel()
      logfile <- config$GetLogFile()
      logout <- config$GetLogOut()
      if (logout == "Console"){
        removeHandler(writeToConsole)
        addHandler(writeToConsole, logger = module, file = logfile, 
                   levle = loglevel)
        
      } else if ( logout == "File"){
        if (file.exists(logfile)){
          file.rename(from = logfile, 
                      to = paste0(config$GetDirLog(), 
                                  as.character(Sys.time()),
                                  ".log"))
        }
        addHandler(writeToFile, logger = module, file = logfile, 
                   levle = loglevel)
      }
      setLevel(loglevel, module)
      # setMsgComposer(LogComposer, module)
      
      LogDebug(paste0("Init ", module, ", log writeTo", logout," ", logfile))
    },
    
    
    ColumnProcessDataFrame = function(df, job) {
      # select some column from dataframe
      if (length(job$select) > 0) {
        df <- df[job$select]
      }
      
      # drop some column from dataframe
      df[job$drop] <- NULL
      
      # rename some column from dataframe
      j <- 1
      while (j <= length(job$rename)){
        name.list <- job$rename[[j]]
        names(df) <- sub(paste0("^", name.list$name, "$"), 
                          name.list$rename, names(df))
        j <- j + 1
      }
      
      # keep some column from dataframe
      if (length(job$keep) > 0) {
        df <- df[, job$keep]
      }
      
      # add some column from dataframe
      j <- 1
      while (j <= length(job$add)){
        name <- job$add[[j]]$name
        value <- job$add[[j]]$value
        df[, name] <- value
        j <- j + 1
      }
      
      # remove duplicate rows
      j <- 1
      while (j <= length(job$unique)){
        column <- job$unique[[j]]
        df <- df[!duplicated(df[, column]) ,]
        j <- j + 1
      }
      
      # set data type
      j <- 1
      while (j <= length(job$type)) {
        column <- job$type[[j]]$name
        type <- job$type[[j]]$type
        if (type == kDataTypeCharacter) {
          df[, column] <- as.character(df[, column])
        } else if (type == kDataTypeNumeric) {
          df[, column] <- as.numeric(df[, column])
        } else if (type == kDataTypeBoolean) {
          
        }
        j <- j+1
      }
      return(df)
    },
    
    FilteringDataframe = function(df, filter.name, 
                                  filter.type, filter.value){
      n1 <- nrow(df)
      filter.df <- data.frame()
      if (filter.name == kFilterALL) {
        filter.df <- df
      } else {
        filter.value <- unlist(strsplit(filter.value, kSeparator))
        for (i in 1:length(filter.value)) {
          value <- filter.value[i]
          if (filter.type == kDataTypeCharacter) {
            tmp.df <- df[df[, filter.name] == value, ]
          } else if (filter.type == kDataTypeNumeric) {
            value <- as.numeric(value)
            tmp.df <- df[df[, filter.name] == value, ]
          } else if (filter.type == kDataTypeBoolean){
            if (value == "TRUE") {
              tmp.df <- df[df[, filter.name] == TRUE, ]
            } else if (value == "FALSE") {
              tmp.df <- df[df[, filter.name] == FALSE, ]
            }
          }
          filter.df <- rbind(filter.df, tmp.df)
        }
        
      } 
      n2 <- nrow(filter.df)
      LogDebug(paste("FilteringDataframe", n1, filter.name, filter.value, n2))
      return(filter.df)
    }
    
  )
)