## GreenIndexBase, the parent of all green index class except GreenIndexConfig

## flag for source loaded checking
base.loaded <- TRUE


## difine basic class
GreenIndexBase <- setRefClass(
  "GreenIndexBase",
  
  fields = list(
    module = "character",
    config = "GreenIndexConfig",
    job = "list",
    df = "data.frame"
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
        addHandler(writeToConsole, logger = module, file = logfile, 
                   levle = loglevel)
      } else if ( logout == "File"){
        addHandler(writeToFile, logger = module, file = logfile, 
                   levle = loglevel)
      }
      setLevel(loglevel, module)
      # setMsgComposer(LogComposer, module)
      
      LogDebug(paste0("Init ", module, ", log writeTo", logout," ", logfile))
    },
    
    # some basic method for column operation
    AddColumn = function() {
      j <- 1
      while (j <= length(job$add)){
        name <- job$add[[j]]$name
        value <- job$add[[j]]$value
        df[, name] <<- value
        j <- j + 1
      }
    },
    
    DropColumn = function() {
      df[job$drop] <<- NULL
    },
    
    RenameColumn = function() {
      j <- 1
      while (j <= length(job$rename)){
        name.list <- job$rename[[j]]
        names(df) <<- sub(paste0("^", name.list$name, "$"), 
                         name.list$rename, names(df))
        j <- j + 1
      }
    },
    
    KeepColumn = function() {
      if (length(job$keep) > 0) {
        df <<- df[, job$keep]
      }
    },
    
    ColumnProcess = function() {
      DropColumn()
      RenameColumn()
      KeepColumn()
      AddColumn()
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
      
      return(df)
    }
    
  )
)