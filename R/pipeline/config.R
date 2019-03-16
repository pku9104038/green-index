## Configurations for green index data processing and report generation

## flag for source loaded checking
config.loaded <- TRUE


# Reference Class of Configuration
library(yaml)
library(methods)
GreenIndexConfig <- setRefClass(
  "GreenIndexConfig",
  fields = list(
    module = "character",
    config = "list",
    job = "list"
  ),
  
  methods = list(
    
    # Log
    LogInfo = function(msg, format = ""){
      loginfo(msg, format, logger = module)
    },
    LogWarn = function(msg, format = ""){
      logwarn(msg, format, logger = module)
    },
    LogDebug = function(msg, format = ""){
      logdebug(msg, format, logger = module)
    },
    
    # load config yaml
    LoadYaml = function(f){
      return(yaml.load_file(f))
    },
    LoadConfig = function(f){
      config <<- LoadYaml(f)
    },
    GetConfig = function(){
      return(config)
    },
    
    # Get assessment job
    GetAssessmentJob = function(){
      return(config$asmt$job)
    },
    
    # Get directories
    GetDirLog = function(){
      return(paste0(config$dir$log, GetAssessmentJob(),"/"))
    },
    GetDirYaml = function(){
      return(paste0(config$dir$yaml, GetAssessmentJob(),"/"))
    },
    GetDirDataIn = function(){
      return(paste0(config$dir$data.in, GetAssessmentJob(),"/"))
    },
    
    # Get logger config
    GetLogLevel = function(){
      return(config$log$level)
    },
    GetLogFile = function(){
      logfile <- paste0(GetDirLog(),
                        format(Sys.Date(), format = "%Y-%m-%d"), 
                        ".log")
      return(logfile)
      
    },
    GetLogOut = function(){
      return(config$log$out)
    },
    
   
    
    # Get Database config
    GetDatabasehost = function(){
      return(config$database$host)
    },
    GetDatabasePort = function(){
      return(config$database$port)
    },
    GetDatabaseName = function(){
      # use assessment job as database name
      return(GetAssessmentJob())
    },
    
    # Get yaml config
    GetConsolidateYaml = function(){
      return(config$consolidate$yaml)
    },
    
    # Init 
    Init = function(module.name, config.yaml){
      LoadConfig(config.yaml)
      
      module <<- module.name
      loglevel <- GetLogLevel()
      loginfo(paste("Log Level:", loglevel))
      logfile <- GetLogFile()
      logout <- GetLogOut()
      if (logout == "Console"){
        addHandler(writeToConsole, logger = module, file = logfile,
                   levle = loglevel)
      } else if ( logout == "File"){
        addHandler(writeToFile, logger = module, file = logfile, 
                   levle = loglevel)
      }
      
      setLevel(loglevel, module)
      LogInfo(paste0("Init ", module,", log writeTo", logout, " ", logfile))
    },
    
    InitJob = function(yaml.file){
      job <<- yaml.load_file(yaml.file)
    },
    
    # Get load data job
    GetLoadDataJob = function(){
      return(job$loaddata)
    }
  )
)

