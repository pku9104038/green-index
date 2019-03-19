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
        addHandler(writeToConsole, logger = module, file = logfile, 
                   levle = loglevel)
      } else if ( logout == "File"){
        addHandler(writeToFile, logger = module, file = logfile, 
                   levle = loglevel)
      }
      setLevel(loglevel, module)
      # setMsgComposer(LogComposer, module)
      
      LogInfo(paste0("Init ", module, ", log writeTo", logout," ", logfile))
    }
  )
)