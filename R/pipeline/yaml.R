## Yaml class for green index data processing and report generation


## flag for source loaded checking
yaml.loaded <- TRUE


## define GreenIndexXlsx
library(methods)

GreenIndexYaml <- setRefClass(
  "GreenIndexYaml",
  contains = "GreenIndexBase",
  
  fields = list(
   
  ),
  
  methods = list(
    
    ConsolidateYaml = function(){
      dir <- config$GetDirYaml()
      yaml.list <- config$GetConsolidateYaml()
      yaml.out <- paste0(dir, config$GetAssessmentJob(), ".yaml")
      
      outputfile <- file(yaml.out, 'w+')
      for (i in 1:length(yaml.list)) {
        inputfile <- file(paste0(dir,yaml.list[[i]]), 'rt')
        text <- readLines(inputfile)
        LogInfo(paste("Consolidate", yaml.list[[i]]))
        writeLines(text, outputfile)
        close(inputfile)
      }
      close(outputfile)
      
      config$InitJob(yaml.out)
      
    }
    
  )
)