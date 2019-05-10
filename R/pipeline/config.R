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
    config.yaml = "character",
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
    # Get assessment name
    GetAssessmentName = function(){
      return(config$asmt$name)
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
    GetDirDataOut = function(){
      return(paste0(config$dir$data.out, GetAssessmentJob(),"/"))
    },
    GetDirReportIn = function(){
      return(paste0(config$dir$report.in, GetAssessmentJob(),"/"))
    },
    GetDirReportOut = function(){
      return(paste0(config$dir$report.out, GetAssessmentJob(),"/"))
    },
    GetDirFigure = function(){
      return(config$dir$figure)
    },
    GetDirRmarkdown = function(){
      return(config$dir$rmarkdown)
    },
    GetDirCommon = function(){
      return(config$dir$common)
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
      return(config$jobs$yaml)
    },
    
    # Check reworkall
    IsReworkAll = function(){
      return(config$reworkall)
    },
    
    IsDropData = function(){
      return(config$dropdata)
    },
    
    # Init 
    Init = function(module.name, config.file){
      config.yaml <<- config.file
      
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
      
      InitJobs()
    },
    
    #InitJobs = function(yaml.file){
    #  job <<- yaml.load_file(yaml.file)
    #},
    
    InitJobs = function() {
    #ConsolidateJob = function(){
      dir <- GetDirYaml()
      yaml.list <- GetConsolidateYaml()
      yaml.out <- paste0(dir, GetAssessmentJob(), ".yaml")
      
      outputfile <- file(yaml.out, 'w+')
      inputfile <- file(config.yaml, 'rt')
      text <- readLines(inputfile)
      LogDebug(paste("Consolidate", config.yaml))
      writeLines(text, outputfile)
      close(inputfile)
      for (i in 1:length(yaml.list)) {
        inputfile <- file(paste0(dir,yaml.list[[i]]), 'rt')
        text <- readLines(inputfile)
        LogDebug(paste("Consolidate", yaml.list[[i]]))
        writeLines(text, outputfile)
        close(inputfile)
      }
      close(outputfile)
      LogDebug(yaml.out)
      job <<- yaml.load_file(yaml.out)
      
      #InitJobs(yaml.out)
      
    },
    
    # Get load data job
    GetLoadDataJob = function(){
      return(job$loaddata)
    },
    
    # Get dictionary job
    GetDictionaryJob = function(){
      return(job$dictionary)
    },
    
    # Get check data job
    GetCheckDataJob = function(){
      return(job$checkdata)
    },
    
    # Get join data job
    GetJoinDataJob = function(){
      return(job$joindata)
    },
    
    # Get split data job
    GetSplitDataJob = function(){
      return(job$splitdata)
    },
    
    # Get clean data job
    GetCleanDataJob = function(){
      return(job$cleandata)
    },
    
    # Get assign point
    GetAssignPointJob = function(){
      return(job$assignment)
    },
    
    # Get transform data
    GetTransformJob = function(){
      return(job$transform)
    },
    
    # Get pipeline job
    GetPipelineJob = function(job.name){
      return(job[job.name])
    },
    
    GetConfigJob = function() {
      return(job)
    }
    
  )
)

